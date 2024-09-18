package com.schuwalow.zio.stmt

import zio.ZLayer
import zio.test.{TestAspect, ZIOSpecDefault}

object CounterSpec extends ZIOSpecDefault {
  def spec = suite("Counter")(
    checkConsistencyWithModelLayer(ZLayer(Counter.makeOverflowing), CounterModel) @@ TestAspect.failing,
    checkConsistencyWithModelLayer(ZLayer(Counter.makeCorrect), CounterModel),
    checkLineralizabilityLayer(ZLayer(Counter.makeNotThreadsafe), CounterModel) @@ TestAspect.failing,
    checkLineralizabilityLayer(ZLayer(Counter.makeCorrect), CounterModel)
  )
}
