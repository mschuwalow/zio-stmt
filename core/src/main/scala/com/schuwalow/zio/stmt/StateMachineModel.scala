package com.schuwalow.zio.stmt

import zio._
import zio.test.Gen

trait DispatchableCommand[R, RealThing, Model] {
  type Response

  def dispatch(realThing: RealThing): RIO[R, Response]
  def dispatchModel(m: Model): (Model, Response)
}

abstract class StateMachineModel[R, RealThing] {

  type Model

  protected trait BaseCommand extends DispatchableCommand[R, RealThing, Model]

  type Command <: BaseCommand

  def initModel: Model

  def generateCommand: Gen[R, Command]

  def concurrentSave(model: Model, commands: List[Command]): Boolean

  final def generateProgram: Gen[R, List[Command]] =
    Gen.listOf(generateCommand)

  final def generateConcurrentProgram(
    minConcurrentSteps: Int = 2,
    maxConcurrentSteps: Int = 5
  ): Gen[R, List[List[Command]]] = {
    def advanceModel(model: Model, commands: List[Command]): Model =
      commands.foldLeft(model)(step(_, _)._1)

    def go(model: Model, size: Int, acc: List[List[Command]]): Gen[R, List[List[Command]]] =
      if (size <= 0) Gen.const(acc.reverse)
      else
        for {
          n        <- Gen.int(minConcurrentSteps, maxConcurrentSteps)
          commands <- Gen.listOfN(n)(generateCommand)
          if concurrentSave(model, commands)
          nextModel = advanceModel(model, commands)
          result   <- go(nextModel, size - n, commands :: acc)
        } yield result

    Gen.sized { size =>
      go(initModel, size, Nil)
    }
  }

  final def step(model: Model, command: Command): (Model, command.Response) = command.dispatchModel(model)
}
