package com.schuwalow.zio.stmt

import zio.ZLayer
import zio.test.{TestAspect, ZIOSpecDefault}

object CounterSpec extends ZIOSpecDefault {
  def spec = suite("Counter")(
    checkConsistencyWithModel(ZLayer(Counter.makeOverflowing), CounterModel) @@ TestAspect.failing,
    checkConsistencyWithModel(ZLayer(Counter.makeCorrect), CounterModel),
    checkLinearizability(ZLayer(Counter.makeNotThreadsafe), CounterModel) @@ TestAspect.failing,
    checkLinearizability(ZLayer(Counter.makeCorrect), CounterModel)
  )
}
