package com.schuwalow.zio.stmt

import zio.internal.stacktracer.SourceLocation
import zio.test._
import zio.{test => _, _}

trait TestConstructors {

  def checkModel[R, A: Tag, M <: StateMachineModel[R, A]: Tag](makeRealThing: RLayer[R, A], smm: M)(implicit
    sl: SourceLocation,
    t: Trace
  ): Spec[R, Throwable] =
    test(s"${Tag[A].tag.repr} is compatible with model ${Tag[M].tag.repr}") {
      check(smm.generateProgram) { program =>
        ZIO
          .serviceWithZIO[A](smm.assertConsistencyWithModel(_, smm.initModel, program))
          .provideSomeLayer[R](makeRealThing)
      }
    }

  def checkLineralizability[R, A: Tag, M <: StateMachineModel[R, A]: Tag](makeRealThing: RLayer[R, A], smm: M)(implicit
    sl: SourceLocation,
    t: Trace
  ): Spec[R, Throwable] =
    test(s"${Tag[A].tag.repr} fulfills linearizablity according to model ${Tag[M].tag.repr}") {
      check(smm.generateConcurrentProgram()) { program =>
        ZIO
          .serviceWithZIO[A](smm.assertLineralizability(_, smm.initModel, program))
          .provideSomeLayer[R](makeRealThing)
      }
    }
}
