package com.schuwalow.zio.stmt

import zio.ZLayer
import zio.test.{TestAspect, ZIOSpecDefault}

object CounterSpec extends ZIOSpecDefault {
  def spec = suite("Counter")(
    checkModel(CounterStateMachineModel, ZLayer(Counter.make)) @@ TestAspect.failing
  )
}
