package com.schuwalow.zio.stmt

import zio._
import scala.util.Try

trait Counter {
  def increment(amount: Int): UIO[Unit]
  val get: UIO[Int]
}

object Counter {
  private final class CorrectCounter(ref: Ref[Int]) extends Counter {

    def increment(amount: Int): UIO[Unit] = ref.update(n => Try(Math.addExact(n, amount)).getOrElse(Int.MaxValue))

    val get: UIO[Int] = ref.get

  }

  private final class OverflowingCounter(ref: Ref[Int]) extends Counter {

    def increment(amount: Int): UIO[Unit] = ref.update(_ + amount)

    val get: UIO[Int] = ref.get

  }

  private final class NotThreadsafeCounter(ref: Ref[Int]) extends Counter {

    def increment(amount: Int): UIO[Unit] = ref.get.flatMap(n => ref.set(n + amount))

    val get: UIO[Int] = ref.get
  }

  val makeCorrect: UIO[Counter] = Ref.make(0).map(new CorrectCounter(_))

  val makeOverflowing: UIO[Counter] = Ref.make(0).map(new OverflowingCounter(_))

  val makeNotThreadsafe: UIO[Counter] = Ref.make(0).map(new NotThreadsafeCounter(_))
}
