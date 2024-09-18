package com.schuwalow.zio.stmt

import zio.internal.stacktracer.SourceLocation
import zio.test._
import zio.{test => _, _}

trait TestConstructors {

  def checkConsistencyWithModelLayer[R, A: Tag, M <: StateMachineModel[R, A]: Tag](layer: RLayer[R, A], smm: M)(implicit
    sl: SourceLocation,
    t: Trace
  ): Spec[R, Throwable] =
    test(s"${Tag[A].tag.repr} is compatible with model ${Tag[M].tag.repr}") {
      check(smm.generateProgram) { program =>
        ZIO
          .serviceWithZIO[A](smm.validateConsistencyWithModel(_, smm.initialState, program))
          .provideSomeLayer[R](layer)
      }
    }

  def checkConsistencyWithModel[R, A: Tag, M <: StateMachineModel[R, A]: Tag](implementation: A, smm: M)(implicit
    sl: SourceLocation,
    t: Trace
  ): Spec[R, Throwable] =
    checkConsistencyWithModelLayer[R, A, M](ZLayer.succeed(implementation), smm)

  def checkLineralizabilityLayer[R, A: Tag, M <: StateMachineModel[R, A]: Tag](
    layer: RLayer[R, A],
    smm: M,
    programGenerationSettings: ConcurrentProgramGenerationSettings = ConcurrentProgramGenerationSettings.default
  )(implicit sl: SourceLocation, t: Trace): Spec[R, Throwable] =
    test(s"${Tag[A].tag.repr} fulfills linearizablity according to model ${Tag[M].tag.repr}") {
      check(smm.generateConcurrentProgram(programGenerationSettings)) { program =>
        ZIO
          .serviceWithZIO[A](smm.validateLineralizability(_, smm.initialState, program))
          .provideSomeLayer[R](layer)
      }
    }

  def checkLineralizability[R, A: Tag, M <: StateMachineModel[R, A]: Tag](
    implementation: A,
    smm: M,
    programGenerationSettings: ConcurrentProgramGenerationSettings = ConcurrentProgramGenerationSettings.default
  )(implicit
    sl: SourceLocation,
    t: Trace
  ): Spec[R, Throwable] =
    checkLineralizabilityLayer[R, A, M](ZLayer.succeed(implementation), smm, programGenerationSettings)
}
