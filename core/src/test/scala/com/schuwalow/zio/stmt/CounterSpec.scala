package com.schuwalow.zio.stmt

import zio.ZLayer
import zio.test.{TestAspect, ZIOSpecDefault}

object CounterSpec extends ZIOSpecDefault {
  def spec = suite("Counter")(
    checkModel(CounterModel, ZLayer(Counter.makeOverflowing)) @@ TestAspect.failing,
    checkModel(CounterModel, ZLayer(Counter.makeCorrect)),
    checkLineralizability(CounterModel, ZLayer(Counter.makeNotThreadsafe)) @@ TestAspect.failing,
    checkLineralizability(CounterModel, ZLayer(Counter.makeCorrect))
  )
}
