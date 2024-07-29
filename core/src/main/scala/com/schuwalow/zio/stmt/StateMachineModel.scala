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

  final def generateProgram: Gen[R, List[Command]] =
    Gen.listOf(generateCommand)

  final def step(model: Model, command: Command): (Model, command.Response) = command.dispatchModel(model)
}
