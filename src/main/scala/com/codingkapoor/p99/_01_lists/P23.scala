package com.codingkapoor.p99._01_lists

import scala.util.Random

/**
 * 
 * P23 (**) Extract a given number of randomly selected elements from a list.
 * Example:
 *
 * scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
 * res0: List[Symbol] = List('e, 'd, 'a)
 *
 * [Hint: Use the solution to problem P20]
 * 
 */
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