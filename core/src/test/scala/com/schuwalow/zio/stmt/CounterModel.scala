package com.schuwalow.zio.stmt

import zio.test.Gen

object CounterModel extends StateMachineModel[Any, Counter] {

  type ModelState = BigInt

  sealed trait Command extends BaseCommand

  final case class Increment(amount: Int) extends Command {
    type Response = Unit

    def dispatch(counter: Counter) =
      counter.increment(amount)

    def advanceModel(model: ModelState) =
      if (model + amount > Int.MaxValue) (Int.MaxValue, ()) else (model + amount, ())
  }

  object Get extends Command {
    type Response = Int

    def dispatch(counter: Counter) =
      counter.get

    def advanceModel(model: ModelState) =
      (model, model.toInt)
  }

  def generateCommand(model: ModelState) =
    Gen.oneOf(
      Gen.int(0, Int.MaxValue).map(Increment(_)),
      Gen.const(Get)
    )

  def initialState = BigInt(0)

  def concurrentSave(model: ModelState, commands: List[Command]) = true
}
