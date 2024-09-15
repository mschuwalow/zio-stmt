package com.schuwalow.zio.stmt

import scala.annotation.tailrec

final case class RoseTree[+A](value: A, children: List[RoseTree[A]])

object RoseTree {
  def singleton[A](a: A): RoseTree[A] = RoseTree(a, Nil)

  def unfold[A, S](s: S)(f: S => (A, List[S])): RoseTree[A] = {
    // TODO: @tailrec
    def go(s: S): RoseTree[A] = {
      val (a, ss) = f(s)
      RoseTree(a, ss.map(go))
    }
    go(s)
  }

  def unfoldForest[A, S](s: S)(f: S => List[(A, S)]): Forest[A] = {
    def go(s: S): Forest[A] = {
      f(s).map { case (a, ss) =>
        RoseTree(a, go(ss))
      }
    }
    go(s)
  }
}
