package com.schuwalow.zio.stmt

import zio.internal.stacktracer.SourceLocation
import zio.test.Assertion.Arguments.valueArgument
import zio.test.Assertion.equalTo
import zio.test.{ErrorMessage => M, _}
import zio.{test => _, _}

object ConcurrentCheck {

  def checkSequentialProgram[R, A](
    implementation: A,
    smm: StateMachineModel[R, A]
  )(
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
    go(smm.initModel, program, Nil)
  }

  def checkConcurrentProgram[R, A](
    realThing: A,
    smm: StateMachineModel[R, A]
  )(
    program: List[List[smm.Command]]
  )(implicit
    sl: SourceLocation,
    t: Trace
  ): RIO[R, TestResult] = {
    type CommandAndResponseF[+T <: smm.Command] = (T, T#Response)
    type CommandAndResponse = CommandAndResponseF[smm.Command]


    sealed trait Operation

    case class Invoke(fiberId: FiberId) extends Operation
    case class Complete(fiberId: FiberId, cnr: CommandAndResponse) extends Operation

    type History = List[Operation]

    type Interleavings = Forest[CommandAndResponse]

    def allInterleavings(history: History): Interleavings = {
      def takeInvocations(history: History, acc: List[FiberId] = Nil): List[FiberId] =
        history match {
          case Invoke(fiberId) :: xs => takeInvocations(xs, fiberId :: acc)
          case _ => acc.reverse
        }

      def findResponse(fiberId: FiberId, history: History, acc: History = Nil): Option[(CommandAndResponse, History)] = {
        history match {
          case Nil => None
          case Complete(`fiberId`, cnr) :: xs => Some((cnr, acc.reverse ++ xs))
          case x :: xs => findResponse(fiberId, xs, x :: acc)
        }
      }

      RoseTree.unfoldForest(history)(history => takeInvocations(history).flatMap(findResponse(_, history)))
    }

    def linearizable(interleavings: Interleavings, model: smm.Model): Boolean = {
      def go(stack: List[(smm.Model, Interleavings)]): Boolean =
        stack match {
          case Nil => false
          case (_, Nil) :: _ => true
          case (model, interleavings) :: xs =>
            val next = interleavings.flatMap {
              case RoseTree((command, response), children) =>
                val (nextModel, modelResponse) = command.dispatchModel(model)
                if (response != modelResponse) Nil
                else List((nextModel, children))
            }
            go(next ++ xs)
        }
      go(List((model, interleavings)))
    }

    ???
  }
}
