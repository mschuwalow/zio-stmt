package com.schuwalow.zio.stmt

import zio.internal.stacktracer.SourceLocation
import zio.test.Assertion.Arguments.valueArgument
import zio.test.{ErrorMessage => M}
import zio.test.{Gen, TestResult, _}
import zio.{test => _, _}

import scala.annotation.tailrec
import scala.collection.mutable

trait DispatchableCommand[R, RealThing, Model] {
  type Response

  def dispatch(realThing: RealThing): RIO[R, Response]
  def dispatchModel(m: Model): (Model, Response)
}

abstract class StateMachineModel[R, RealThing] {

  type Model

  protected trait BaseCommand extends DispatchableCommand[R, RealThing, Model]

  type Command <: BaseCommand

  def initModel: Model

  def generateCommand(model: Model): Gen[R, Command]

  def concurrentSave(model: Model, commands: List[Command]): Boolean

  // internal api

  private[stmt] final def generateProgram: Gen[R, List[Command]] =
    Gen.unfoldGen(initModel) { model =>
      for {
        command       <- generateCommand(model)
        (nextModel, _) = step(model, command)
      } yield (nextModel, command)
    }

  private[stmt] final def generateConcurrentProgram(
    minProgramSize: Int = 2,
    maxProgramSize: Int = 1000,
    minConcurrentSteps: Int = 2,
    maxConcurrentSteps: Int = 5
  ): Gen[R, List[List[Command]]] = {
    def advanceModel(model: Model, commands: List[Command]): Model =
      commands.foldLeft(model)(step(_, _)._1)

    def go(model: Model, size: Int, acc: List[List[Command]]): Gen[R, List[List[Command]]] =
      if (size <= 0) Gen.const(acc.reverse)
      else
        for {
          n        <- Gen.int(minConcurrentSteps, maxConcurrentSteps)
          commands <- Gen.listOfN(n)(generateCommand(model))
          if concurrentSave(model, commands)
          nextModel = advanceModel(model, commands)
          result   <- go(nextModel, size - n, commands :: acc)
        } yield result

    Gen.int(minProgramSize, maxProgramSize).flatMap(go(initModel, _, Nil))
  }

  private[stmt] final def assertConsistencyWithModel(
    implementation: RealThing,
    model: Model,
    program: List[Command]
  )(implicit
    sl: SourceLocation,
    t: Trace
  ): RIO[R, TestResult] = {
    def equalToModelResponse[A](
      modelResponse: A,
      model: Model,
      currentCommand: Command,
      commandHistory: List[Command]
    ): Assertion[A] =
      Assertion(
        TestArrow
          .make[A, Boolean] { provided =>
            val baseMessage =
              (M.text("response") + M.pretty(provided) + M.did + "match expected response" + M.pretty(modelResponse)) ++
                (M.text("command:") + M.pretty(currentCommand)) ++
                (M.text("model state:") + M.pretty(model))

            val historyMessage =
              if (commandHistory.isEmpty)
                M.text("no previous commands")
              else
                commandHistory.reverse.foldLeft(M.text("command history:")) { case (acc, next) =>
                  acc ++ (M.text("  *") + M.pretty(next))
                }

            TestTrace.boolean(provided == modelResponse)(baseMessage ++ historyMessage)
          }
          .withCode(
            "matchesModelResponse",
            valueArgument(modelResponse, name = Some("modelResponse")),
            valueArgument(model, name = Some("model")),
            valueArgument(currentCommand, name = Some("currentCommand")),
            valueArgument(commandHistory, name = Some("commandHistory"))
          )
      )

    def go(model: Model, commands: List[Command], trace: List[Command]): RIO[R, TestResult] =
      commands match {
        case Nil                       => assertCompletesZIO
        case command :: futureCommands =>
          for {
            realResponse              <- command.dispatch(implementation)
            (nextModel, modelResponse) = command.dispatchModel(model)
            assertionResult            = assert(realResponse)(equalToModelResponse(modelResponse, model, command, trace))
            result                    <- if (assertionResult.isSuccess)
                                           go(nextModel, futureCommands, command :: trace)
                                         else
                                           ZIO.succeed(assertionResult)
          } yield result
      }
    go(model, program, Nil)
  }

  private[stmt] final def assertLineralizability(
    implementation: RealThing,
    model: Model,
    program: List[List[Command]]
  )(implicit
    sl: SourceLocation,
    t: Trace
  ): RIO[R, TestResult] = {
    sealed trait Operation

    case class Invoke(id: Int, command: Command) extends Operation
    case class Return(id: Int, response: Any)    extends Operation

    type History = List[Operation]

    def linearizable(history: History, model: Model): Boolean = {
      if (history.isEmpty) return true

      @tailrec
      def findReturn(id: Int, history: History, acc: History = Nil): (Any, History) =
        history match {
          case Nil                         => throw new IllegalStateException("No response found")
          case (Return(`id`, value)) :: xs => (value, acc.reverse ++ xs)
          case x :: xs                     => findReturn(id, xs, x :: acc)
        }

      @tailrec
      def takeConcurrentCommands(
        history: History,
        previousHistory: History = Nil,
        acc: List[(Command, Any, History)] = Nil
      ): List[(Command, Any, History)] =
        history match {
          case (op @ Invoke(id, command)) :: xs =>
            val (result, remainingHistory) = findReturn(id, xs)
            takeConcurrentCommands(
              xs,
              op :: previousHistory,
              (command, result, previousHistory.reverse ++ remainingHistory) :: acc
            )
          case _                                => acc.reverse
        }

      val stack = mutable.Stack.empty[(Model, Command, Any, History)]

      def pushNextStates(model: Model, history: History): Unit =
        for ((command, response, remainingHistory) <- takeConcurrentCommands(history))
          stack.push((model, command, response, remainingHistory))

      pushNextStates(model, history)

      var found = false
      while (!found && stack.nonEmpty) {
        val (model, command, response, history) = stack.pop()
        val (nextModel, modelResponse)          = command.dispatchModel(model)

        if (response == modelResponse) {
          if (history.isEmpty) found = true
          else pushNextStates(nextModel, history)
        }
      }

      found
    }

    def isLinearizable(model: Model): Assertion[History] =
      Assertion(
        TestArrow
          .make[History, Boolean] { provided =>
            val resultMessage = M.text("history") + M.did + "linearize."
            TestTrace.boolean(linearizable(provided, model))(resultMessage)
          }
          .withCode(
            "isLinearizable",
            valueArgument(model, name = Some("model"))
          )
      )

    def runConcurrentCommand(command: Command, historyRef: Ref[History]): RIO[R, Unit] =
      for {
        id       <- historyRef.modify { old =>
                      val id = old.size; (id, Invoke(id, command) :: old)
                    }
        response <- command.dispatch(implementation)
        _        <- historyRef.update(Return(id, response) :: _)
      } yield ()

    for {
      historyRef <- Ref.make[History](Nil)
      _          <- ZIO.foreachDiscard(program)(ZIO.foreachParDiscard(_)(runConcurrentCommand(_, historyRef)))
      history    <- historyRef.get.map(_.reverse)
    } yield assert(history)(isLinearizable(model))
  }

  private final def step(model: Model, command: Command): (Model, command.Response) = command.dispatchModel(model)
}
