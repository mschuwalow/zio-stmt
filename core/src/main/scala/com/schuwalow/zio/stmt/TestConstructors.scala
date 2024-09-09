package com.schuwalow.zio.stmt

import zio.internal.stacktracer.SourceLocation
import zio.test.Assertion.Arguments.valueArgument
import zio.test.Assertion.equalTo
import zio.test.{ErrorMessage => M, _}
import zio.{test => _, _}

trait TestConstructors {

  def checkModel[R, A: Tag](smm: StateMachineModel[R, A], makeRealThing: RLayer[R, A])(implicit
    sl: SourceLocation,
    t: Trace
  ): Spec[R, Throwable] =
    test(s"${Tag[A].tag.repr} is compatible with model") {
      check(smm.generateProgram) { program =>
        ZIO
          .serviceWithZIO[A] { realThing =>
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
                      (M.text("response") + M.pretty(provided) + M.did + "match model response" + M
                        .pretty(modelResponse)) ++
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
                    realResponse              <- command.dispatch(realThing)
                    (nextModel, modelResponse) = command.dispatchModel(model)
                    _                         <- assert(realResponse)(equalToModelResponse(modelResponse, model, command, trace))
                    result                    <- go(nextModel, futureCommands, command :: trace)
                  } yield result
              }
            go(smm.initModel, program, Nil)
          }
          .provideSomeLayer[R](makeRealThing)
      }
    }

  def checkLineralizability[R, A: Tag](smm: StateMachineModel[R, A], makeRealThing: RLayer[R, A])(implicit
    sl: SourceLocation,
    t: Trace
  ): Spec[R, Throwable] =
    test(s"${Tag[A].tag.repr} fulfills linearizablity") {
      check(smm.generateConcurrentProgram()) { program =>
        ZIO.serviceWithZIO[A](ModelChecks.checkLineralizability(smm)(_, smm.initModel, program))
      }.provideSomeLayer[R](makeRealThing)
    }
}
