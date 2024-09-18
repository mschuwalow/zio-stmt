package com.schuwalow.zio.stmt

import zio.internal.stacktracer.SourceLocation
import zio.test._
import zio.{test => _, _}

import scala.language.implicitConversions

trait TestConstructors {
  import TestConstructors._

  def checkConsistencyWithModel[R, A: Tag](
    impl: ImplementationArg[R, A],
    smm: StateMachineModel[R, A]
  )(implicit sl: SourceLocation, t: Trace, modelTag: Tag[smm.type]): Spec[R, Throwable] =
    test(s"${Tag[A].tag.repr} is compatible with model ${modelTag.tag.repr}") {
      check(smm.generateProgram) { program =>
        impl.runWithA(smm.validateConsistencyWithModel(_, smm.initialState, program))
      }
    }

  def checkLinearizability[R, A: Tag](
    impl: ImplementationArg[R, A],
    smm: StateMachineModel[R, A],
    programGenerationSettings: ConcurrentProgramGenerationSettings = ConcurrentProgramGenerationSettings.default
  )(implicit sl: SourceLocation, t: Trace, modelTag: Tag[smm.type]): Spec[R, Throwable] =
    test(s"${Tag[A].tag.repr} fulfills linearizablity according to model ${modelTag.tag.repr}") {
      check(smm.generateConcurrentProgram(programGenerationSettings)) { program =>
        impl.runWithA(smm.validateLinearizability(_, smm.initialState, program))
      }
    }
}

object TestConstructors {
  trait ImplementationArg[-R, +A] {
    def runWithA[R1 <: R, E >: Throwable, B](f: A => ZIO[R1, E, B]): ZIO[R1, E, B]
  }

  object ImplementationArg {
    implicit def layerImplementationArg[R, A: Tag](layer: RLayer[R, A]): ImplementationArg[R, A] =
      new ImplementationArg[R, A] {
        def runWithA[R1 <: R, E >: Throwable, B](f: A => ZIO[R1, E, B]): ZIO[R1, E, B] =
          ZIO.serviceWithZIO[A](f).provideSomeLayer[R1](layer)
      }

    implicit def zioImplementationArg[R, A](zio: RIO[R, A]): ImplementationArg[R, A] =
      new ImplementationArg[R, A] {
        def runWithA[R1 <: R, E >: Throwable, B](f: A => ZIO[R1, E, B]): ZIO[R1, E, B] =
          zio.flatMap(f)
      }

    implicit def implementationImplementationArg[A](a: A): ImplementationArg[Any, A] =
      new ImplementationArg[Any, A] {
        def runWithA[R1, E >: Throwable, B](f: A => ZIO[R1, E, B]): ZIO[R1, E, B] = f(a)
      }
  }
}
