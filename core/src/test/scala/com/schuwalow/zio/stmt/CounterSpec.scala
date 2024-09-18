package com.schuwalow.zio.stmt

import zio.ZLayer
import zio.test.{TestAspect, ZIOSpecDefault}

object CounterSpec extends ZIOSpecDefault {
  def spec = suite("Counter")(
    checkModel(ZLayer(Counter.makeOverflowing), CounterModel) @@ TestAspect.failing,
    checkModel(ZLayer(Counter.makeCorrect), CounterModel),
    checkLineralizability(ZLayer(Counter.makeNotThreadsafe), CounterModel) @@ TestAspect.failing,
    checkLineralizability(ZLayer(Counter.makeCorrect), CounterModel)
  )
}
