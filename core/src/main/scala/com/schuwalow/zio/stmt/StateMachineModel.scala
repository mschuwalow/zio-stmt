package com.schuwalow.zio.stmt

import zio.internal.stacktracer.SourceLocation
import zio.test.Assertion.Arguments.valueArgument
import zio.test.{ErrorMessage => M}
import zio.test.{Gen, TestResult, _}
import zio.{test => _, _}

import scala.annotation.tailrec
import scala.collection.mutable

abstract class StateMachineModel[R, RealThing] {

  type ModelState

  protected trait BaseCommand {
    type Response
    def dispatch(realThing: RealThing): RIO[R, Response]
    def advanceModel(m: ModelState): (ModelState, Response)
  }

  type Command <: BaseCommand

  def initialState: ModelState

  def generateCommand(modelState: ModelState): Gen[R, Command]

  def concurrentSave(modelState: ModelState, commands: List[Command]): Boolean

  // internal api

  private[stmt] final def generateProgram: Gen[R, List[Command]] =
    Gen.unfoldGen(initialState) { model =>
      for {
        command       <- generateCommand(model)
        (nextModel, _) = step(model, command)
      } yield (nextModel, command)
    }

  private[stmt] final def generateConcurrentProgram(
    settings: ConcurrentProgramGenerationSettings
  ): Gen[R, List[List[Command]]] = {
    def advanceModel(model: ModelState, commands: List[Command]): ModelState =
      commands.foldLeft(model)(step(_, _)._1)

    def go(model: ModelState, size: Int, acc: List[List[Command]]): Gen[R, List[List[Command]]] =
      if (size <= 0) Gen.const(acc.reverse)
      else
        for {
          n        <- Gen.int(settings.minConcurrentSteps, settings.maxConcurrentSteps)
          commands <- Gen.listOfN(n)(generateCommand(model))
          if concurrentSave(model, commands)
          nextModel = advanceModel(model, commands)
          result   <- go(nextModel, size - n, commands :: acc)
        } yield result

    Gen.int(settings.minProgramSize, settings.maxProgramSize).flatMap(go(initialState, _, Nil))
  }

  private[stmt] final def validateConsistencyWithModel(
    implementation: RealThing,
    modelState: ModelState,
    program: List[Command]
  )(implicit
    sl: SourceLocation,
    t: Trace
  ): RIO[R, TestResult] = {
    def equalToModelResponse[A](
      modelResponse: A,
      modelState: ModelState,
      currentCommand: Command,
      commandHistory: List[Command]
    ): Assertion[A] =
      Assertion(
        TestArrow
          .make[A, Boolean] { provided =>
            val baseMessage =
              (M.text("response") + M.pretty(provided) + M.did + "match expected response" + M.pretty(modelResponse)) ++
                (M.text("command:") + M.pretty(currentCommand)) ++
                (M.text("model state:") + M.pretty(modelState))

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
            valueArgument(modelState, name = Some("model")),
            valueArgument(currentCommand, name = Some("currentCommand")),
            valueArgument(commandHistory, name = Some("commandHistory"))
          )
      )

    def go(modelState: ModelState, commands: List[Command], trace: List[Command]): RIO[R, TestResult] =
      commands match {
        case Nil                       => assertCompletesZIO
        case command :: futureCommands =>
          for {
            realResponse              <- command.dispatch(implementation)
            (nextModel, modelResponse) = command.advanceModel(modelState)
            assertionResult            = assert(realResponse)(equalToModelResponse(modelResponse, modelState, command, trace))
            result                    <- if (assertionResult.isSuccess)
                                           go(nextModel, futureCommands, command :: trace)
                                         else
                                           ZIO.succeed(assertionResult)
          } yield result
      }
    go(modelState, program, Nil)
  }

  private[stmt] final def validateLineralizability(
    implementation: RealThing,
    modelState: ModelState,
    program: List[List[Command]]
  )(implicit
    sl: SourceLocation,
    t: Trace
  ): RIO[R, TestResult] = {
    sealed trait Operation

    case class Invoke(id: Int, command: Command) extends Operation
    case class Return(id: Int, response: Any)    extends Operation

    type History = List[Operation]

    def linearizable(history: History, modelState: ModelState): Boolean = {
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

      val stack = mutable.Stack.empty[(ModelState, Command, Any, History)]

      def pushNextStates(model: ModelState, history: History): Unit =
        for ((command, response, remainingHistory) <- takeConcurrentCommands(history))
          stack.push((model, command, response, remainingHistory))

      pushNextStates(modelState, history)

      var found = false
      while (!found && stack.nonEmpty) {
        val (model, command, response, history) = stack.pop()
        val (nextModel, modelResponse)          = command.advanceModel(model)

        if (response == modelResponse) {
          if (history.isEmpty) found = true
          else pushNextStates(nextModel, history)
        }
      }

      found
    }

    def isLinearizable(modelState: ModelState): Assertion[History] =
      Assertion(
        TestArrow
          .make[History, Boolean] { provided =>
            val resultMessage = M.text("history") + M.did + "linearize."
            TestTrace.boolean(linearizable(provided, modelState))(resultMessage)
          }
          .withCode(
            "isLinearizable",
            valueArgument(modelState, name = Some("model"))
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
    } yield assert(history)(isLinearizable(modelState))
  }

  private final def step(modelState: ModelState, command: Command): (ModelState, command.Response) =
    command.advanceModel(modelState)
}
