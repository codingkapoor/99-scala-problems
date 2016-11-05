package com.codingkapoor.p99._01_lists

import scala.util.Random

object P25 {

  def randomPermute[T](ls: List[T]): List[T] = P23.randomSelectTailRecursive(ls.length, ls)

  def randomPermuteBuiltin[T](ls: List[T]): List[T] = Random.shuffle(ls).toList

  def randomPermuteBuiltinII[T](ls: List[T]): List[T] = {
    val permutations = ls.permutations.toList
    permutations(Random.nextInt(permutations.length))
  }
    
   /*Efficient purely functional algorithms for shuffling are a lot harder.  One
   is described in http://okmij.org/ftp/Haskell/perfect-shuffle.txt using
   Haskell. */

}