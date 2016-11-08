package com.codingkapoor.p99._01_lists

import scala.util.Random

/**
 * 
 * P24 (*) Lotto: Draw N different random numbers from the set 1..M.
 * Example:
 *
 * scala> lotto(6, 49)
 * res0: List[Int] = List(23, 1, 17, 33, 21, 37)
 * 
 */
object P24 {

  def lottoBuiltin[T](count: Int, max: Int): List[Int] = Random shuffle ((1 to max).toList) take (6)
  
  def lottoBuiltinII[T](count: Int, max: Int): List[Int] = Stream.continually(Random.nextInt(max)).distinct.take(count).toList

  def lotto[T](count: Int, max: Int): List[Int] = P23.randomSelectFunctional(count, List.range(1, max + 1))

}
