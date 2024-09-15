package com.schuwalow.zio.stmt

import zio.internal.stacktracer.SourceLocation
import zio.test.Assertion.Arguments.valueArgument
import zio.test.Assertion.{equalTo, isTrue}
import zio.test.{ErrorMessage => M, _}
import zio.{test => _, _}

import scala.annotation.tailrec
import scala.collection.mutable

object ModelChecks {

  def checkConsistencyWithModel[R, A](
    smm: StateMachineModel[R, A]
  )(
    implementation: A,
    model: smm.Model,
    program: List[smm.Command]
  )(implicit
    sl: SourceLocation,
    t: Trace
  ): RIO[R, TestResult] = {
    def equalToModelResponse[A](
      modelResponse: A,
      model: smm.Model,
      currentCommand: smm.Command,
      commandHistory: List[smm.Command]
    ): Assertion[A] =
      Assertion(
        TestArrow
          .make[A, Boolean] { provided =>
            val result = TestArrow.run(equalTo(modelResponse).arrow, Right(provided))

            val baseMessage =
              (M.text("response") + M.pretty(provided) + M.did + "match model response" + M.pretty(modelResponse)) ++
                (M.text("command:") + M.pretty(currentCommand)) ++
                (M.text("model state:") + M.pretty(model))

            val historyMessage =
              if (commandHistory.isEmpty)
                M.text("no previous commands")
              else
                commandHistory.reverse.foldLeft(M.text("command history:")) { case (acc, next) =>
                  acc ++ (M.text("  *") + M.pretty(next))
                }

            TestTrace.boolean(result.isSuccess)(baseMessage ++ historyMessage)
          }
          .withCode(
            "matchesModelResponse",
            valueArgument(modelResponse, name = Some("modelResponse")),
            valueArgument(model, name = Some("model")),
            valueArgument(currentCommand, name = Some("currentCommand")),
            valueArgument(commandHistory, name = Some("commandHistory"))
          )
      )

    def go(model: smm.Model, commands: List[smm.Command], trace: List[smm.Command]): RIO[R, TestResult] =
      commands match {
        case Nil                       => assertCompletesZIO
        case command :: futureCommands =>
          for {
            realResponse              <- command.dispatch(implementation)
            (nextModel, modelResponse) = command.dispatchModel(model)
            _                         <- assert(realResponse)(equalToModelResponse(modelResponse, model, command, trace))
            result                    <- go(nextModel, futureCommands, command :: trace)
          } yield result
      }
    go(model, program, Nil)
  }

  def checkLineralizability[R, A](
    smm: StateMachineModel[R, A]
  )(
    implementation: A,
    model: smm.Model,
    program: List[List[smm.Command]]
  )(implicit
    sl: SourceLocation,
    t: Trace
  ): RIO[R, TestResult] = {
    type CommandAndResponseF[+T <: smm.Command] = (T, T#Response)
    type CommandAndResponse                     = CommandAndResponseF[smm.Command]

    sealed trait Operation

    case class Begin(fiberId: FiberId)                             extends Operation
    case class Complete(fiberId: FiberId, cnr: CommandAndResponse) extends Operation

    type History = List[Operation]

    def linearizable(history: History, model: smm.Model): Boolean = {
      @tailrec
      def findResponse(fiberId: FiberId, history: History, acc: History = Nil): (CommandAndResponse, History) =
        history match {
          case Nil                            => throw new IllegalStateException("No response found")
          case Complete(`fiberId`, cnr) :: xs => (cnr, acc.reverse ++ xs)
          case x :: xs                        => findResponse(fiberId, xs, x :: acc)
        }

      @tailrec
      def takeConcurrentCommands(
        history: History,
        previousHistory: History = Nil,
        acc: List[(CommandAndResponse, History)] = Nil
      ): List[(CommandAndResponse, History)] =
        history match {
          case (cmd @ Begin(fiberId)) :: xs =>
            val (cnr, remainingHistory) = findResponse(fiberId, xs)
            takeConcurrentCommands(
              xs,
              cmd :: previousHistory,
              (cnr, previousHistory.reverse ++ remainingHistory) :: acc
            )
          case _                            => acc.reverse
        }

      val stack = mutable.Stack.empty[(smm.Model, CommandAndResponse, History)]

      def pushNextStates(model: smm.Model, history: History): Unit =
        for ((cnr, remainingHistory) <- takeConcurrentCommands(history))
          stack.push((model, cnr, remainingHistory))

      pushNextStates(model, history)

      var found = false
      while (!found && stack.nonEmpty) {
        val (model, (command, response), history) = stack.pop()
        val (nextModel, modelResponse)            = command.dispatchModel(model)
        if (response == modelResponse) {
          if (history.isEmpty) found = true
          else pushNextStates(nextModel, history)
        }
      }

      found
    }

    def runConcurrentCommand(command: smm.Command, historyRef: Ref[History]): RIO[R, Unit] =
      for {
        fiberId  <- ZIO.fiberId
        _        <- historyRef.update(Begin(fiberId) :: _)
        response <- command.dispatch(implementation)
        _        <- historyRef.update(Complete(fiberId, (command, response)) :: _)
      } yield ()

    for {
      historyRef <- Ref.make[History](Nil)
      _          <- ZIO.foreachDiscard(program)(ZIO.foreachParDiscard(_)(runConcurrentCommand(_, historyRef)))
      history    <- historyRef.get.map(_.reverse)
    } yield assert(linearizable(history, model))(isTrue)
  }
}
