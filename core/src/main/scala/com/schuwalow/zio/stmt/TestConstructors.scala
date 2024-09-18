package com.schuwalow.zio.stmt

import zio.internal.stacktracer.SourceLocation
import zio.test._
import zio.{test => _, _}

trait TestConstructors {

  def checkModel[R, A: Tag](smm: StateMachineModel[R, A], makeRealThing: RLayer[R, A])(implicit
    sl: SourceLocation,
    t: Trace
  ): Spec[R, Throwable] =
    test(s"${Tag[A].tag.repr} is compatible with model") {
      check(smm.generateProgram) { program =>
        ZIO
          .serviceWithZIO[A](ModelChecks.checkConsistencyWithModel(smm)(_, smm.initModel, program))
          .provideSomeLayer[R](makeRealThing)
      }
    }

  def checkLineralizability[R, A: Tag](smm: StateMachineModel[R, A], makeRealThing: RLayer[R, A])(implicit
    sl: SourceLocation,
    t: Trace
  ): Spec[R, Throwable] =
    test(s"${Tag[A].tag.repr} fulfills linearizablity") {
      check(smm.generateConcurrentProgram()) { program =>
        ZIO
          .serviceWithZIO[A](ModelChecks.checkLineralizability(smm)(_, smm.initModel, program))
          .provideSomeLayer[R](makeRealThing)
      }
    }
}
