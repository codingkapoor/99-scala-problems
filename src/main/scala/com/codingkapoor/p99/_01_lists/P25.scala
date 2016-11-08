package com.codingkapoor.p99._01_lists

/**
 * 
 * P25 (*) Generate a random permutation of the elements of a list.
 * [Hint: Use the solution of problem P23.]
 *
 * Example:
 *
 * scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
 * res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
 * 
 */
object P25 {

  def randomPermute[T](ls: List[T]): List[T] = P23.randomSelectTailRecursive(ls.length, ls)

  
  import scala.util.Random
  
  def randomPermuteBuiltin[T](ls: List[T]): List[T] = Random.shuffle(ls).toList

  def randomPermuteBuiltinII[T](ls: List[T]): List[T] = {
    val permutations = ls.permutations.toList
    permutations(Random.nextInt(permutations.length))
  }
    
   /*Efficient purely functional algorithms for shuffling are a lot harder.  One
   is described in http://okmij.org/ftp/Haskell/perfect-shuffle.txt using
   Haskell. */

}