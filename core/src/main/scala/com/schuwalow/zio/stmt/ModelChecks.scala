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
    type CommandAndResponse = CommandAndResponseF[smm.Command]

    sealed trait Operation

    case class Begin(id: Long) extends Operation
    case class Complete(id: Long, cnr: CommandAndResponse) extends Operation

    type History = List[Operation]

    def linearizable(history: History, model: smm.Model): Boolean = {
      @tailrec
      def findResponse(id: Long, history: History, acc: History = Nil): (CommandAndResponse, History) = {
        history match {
          case Nil => throw new IllegalStateException("No response found")
          case Complete(`id`, cnr) :: xs => (cnr, acc.reverse ++ xs)
          case Begin(`id`) :: xs => findResponse(id, xs, acc)
          case x :: xs => findResponse(id, xs, x :: acc)
        }
      }

      @tailrec
      def takeConcurrentInvocations(history: History, acc: List[Long] = Nil): List[Long] =
        history match {
          case Begin(id) :: xs => takeConcurrentInvocations(xs, id :: acc)
          case _ => acc.reverse
        }

      val stack = mutable.Stack.empty[(smm.Model, CommandAndResponse, History)]

      def pushNextStates(model: smm.Model, history: History): Unit =
        for (id <- takeConcurrentInvocations(history)) {
          val (cnr, remainingHistory) = findResponse(id, history)
          stack.push((model, cnr, remainingHistory))
        }

      pushNextStates(model, history)

      var found = false
      var min = history.size
      while (!found && stack.nonEmpty) {
        val (model, (command, response), history) = stack.pop()
        val (nextModel, modelResponse) = command.dispatchModel(model)
        if (response == modelResponse) {
          min = min min history.size
          if (history.isEmpty) found = true
          else pushNextStates(nextModel, history)
        }
      }

      found
    }

    val programWithIds = program.foldRight((0L, List.empty[List[(Long, smm.Command)]])) { case (commands, (counter, acc)) =>
      val (nextCounter, commandsWithId) = commands.foldRight((counter, List.empty[(Long, smm.Command)])) { case (command, (counter, acc)) =>
        val nextCounter = counter + 1
        (nextCounter, (counter, command) :: acc)
      }
      (nextCounter, commandsWithId :: acc)
    }._2

    def runConcurrentCommand(id: Long, command: smm.Command, historyRef: Ref[History]): RIO[R, Unit] =
      for {
        _        <- historyRef.update(Begin(id) :: _)
        response <- command.dispatch(implementation)
        _        <- historyRef.update(Complete(id, (command, response)) :: _)
      } yield ()

    for {
      historyRef <- Ref.make[History](Nil)
      _ <- ZIO.foreachDiscard(programWithIds)(ZIO.foreachParDiscard(_)( { case (id, cmd) => runConcurrentCommand(id, cmd, historyRef) }))
      history <- historyRef.get.map(_.reverse)
    } yield assert(linearizable(history, model))(isTrue)
  }
}
