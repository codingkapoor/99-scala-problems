package com.codingkapoor.p99._01_lists

import scala.util.Random

object P24 {

  def lottoBuiltin[T](count: Int, max: Int): List[Int] = Random shuffle ((1 to max).toList) take (6)
  
  def lottoBuiltinII[T](count: Int, max: Int): List[Int] = Stream.continually(Random.nextInt(max)).distinct.take(count).toList

  def lotto[T](count: Int, max: Int): List[Int] = P23.randomSelectFunctional(count, List.range(1, max + 1))

}
