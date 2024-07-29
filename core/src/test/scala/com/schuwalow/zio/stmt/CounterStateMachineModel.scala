package com.schuwalow.zio.stmt

import zio.test.Gen

object CounterStateMachineModel extends StateMachineModel[Any, Counter] {

  type Model = BigInt

  sealed trait Command extends BaseCommand

  final case class Increment(amount: Int) extends Command {
    type Response = Unit

    def dispatch(counter: Counter) =
      counter.increment(amount)

    def dispatchModel(model: Model) =
      if (model + amount > Int.MaxValue) (Int.MaxValue, ()) else (model + amount, ())
  }

  object Get extends Command {
    type Response = Int

    def dispatch(counter: Counter) =
      counter.get

    def dispatchModel(model: Model) =
      (model, model.toInt)
  }

  def generateCommand =
    Gen.oneOf(
      Gen.const(Get),
      Gen.int.map(Increment(_))
    )

  def initModel = BigInt(0)
}
