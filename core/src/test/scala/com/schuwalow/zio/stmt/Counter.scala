package com.schuwalow.zio.stmt

import zio._

final class Counter(ref: Ref[Int]) {

  def increment(amount: Int): UIO[Unit] = ref.update(_ + amount)

  val get: UIO[Int] = ref.get

}

object Counter {

  val make: UIO[Counter] = Ref.make(0).map(new Counter(_))

}
