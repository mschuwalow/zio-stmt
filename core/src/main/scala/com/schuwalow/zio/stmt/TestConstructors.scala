package com.schuwalow.zio.stmt

import zio.test.Assertion.equalTo
import zio.test._
import zio.{test => _, _}

trait TestConstructors {

  def checkModel[R, A: Tag](smm: StateMachineModel[R, A], makeRealThing: RLayer[R, A]): Spec[R, Throwable] =
    test(s"test ${Tag[A].tag.repr} is compatible with model") {
      check(smm.generateProgram) { program =>
        ZIO
          .serviceWithZIO[A] { realThing =>
            def makeLabel(model: smm.Model, current: smm.Command, trace: List[smm.Command]) =
              s"""
                 |current command: $current
                 |current model state: $model
                 |command history:
                 |${trace.reverse.map(t => s"  * $t").mkString("\n")}
                 |""".stripMargin

            def go(model: smm.Model, commands: List[smm.Command], trace: List[smm.Command]): RIO[R, TestResult] =
              commands match {
                case Nil     => assertCompletesZIO
                case x :: xs =>
                  for {
                    realResponse              <- x.dispatch(realThing)
                    (nextModel, modelResponse) = x.dispatchModel(model)
                    _                         <- assert(realResponse)(equalTo(modelResponse)) ?? makeLabel(model, x, trace)
                    result                    <- go(nextModel, xs, x :: trace)
                  } yield result
              }
            go(smm.initModel, program, Nil)
          }
          .provideSomeLayer[R](makeRealThing)
      }
    }
}
