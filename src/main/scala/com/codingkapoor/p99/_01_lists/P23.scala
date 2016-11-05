package com.codingkapoor.p99._01_lists

import scala.util.Random

object P23 {

  def randomSelectFunctional[T](r: Int, ls: List[T]): List[T] = {
    if (ls == Nil) Nil
    else Seq.fill(r)(Random.nextInt(ls.size)).toList map { i => ls(i) }
  }

  def randomSelectTailRecursive[T](r: Int, ls: List[T]): List[T] = {

    def randomSelectR(t: Int, result: List[T]): List[T] = {
      val (xs, x) = P20.removeAtBuiltin[T](Random.nextInt(ls.size), ls)

      if (t == 0) {
        result
      } else {
        randomSelectR(t - 1, x :: result)
      }
    }

    if (ls == Nil) Nil
    else randomSelectR(r, Nil)
  }

}