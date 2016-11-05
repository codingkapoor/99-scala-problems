package com.codingkapoor.p99._01_lists

import scala.util.Random

object P23 extends App {

  def randomSelectFunctional[T](r: Int, ls: List[T]): List[T] = {
    if (ls == Nil) Nil
    else Seq.fill(r)(Random.nextInt(ls.size)).toList map { i => ls(i) }
  }

  def randomSelectTailRecursive[T](r: Int, ls: List[T]): List[Any] = {

    def randomSelectR[T](t: Int, result: List[T], list: List[T]): List[Any] = {
      val (xs, x) = P20.removeAtBuiltin(Random.nextInt(ls.size), ls)

      if (t == 0) {
        result
      } else {
        randomSelectR(t - 1, x :: result, xs)
      }
    }

    if (ls == Nil) Nil
    else randomSelectR(r, Nil, ls)
  }

}