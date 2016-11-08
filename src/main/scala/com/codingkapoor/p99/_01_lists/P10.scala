package com.codingkapoor.p99._01_lists

/**
 * 
 * P10 (*) Run-length encoding of a list.
 * [Use the result of problem P09 to implement the so-called run-length encoding data compression method. 
 * Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.]
 * 
 * Example:
 * 
 * scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
 * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
 * 
 */
object P10 {

  import P09._

  def encodeFunctional[T](ls: List[T]): List[(Int, T)] = packTailRecursive(ls) map { l => (l.length, l.head) }

  def encodeFunctionalII[T](ls: List[T]): List[(Int, T)] = {
    val xs = packTailRecursive(ls)
    (xs map { l => l.length }) zip (xs map { l => l.head })
  }

  def encodeFunctionalIII[T](ls: List[T]): List[(Int, T)] =
    (packTailRecursive(ls) foldLeft (Nil: List[(Int, T)])) { (acc, x) => acc ::: List((x.length, x.head)) }

  def encodeRecursive[T](ls: List[T]): List[(Int, T)] = {
    
    def encodeR[T]: List[List[T]] => List[(Int, T)] = {
      case Nil      => Nil
      case xs :: ys => (xs.length, xs.head) :: encodeR(ys)
    }

    encodeR(packTailRecursive(ls))
  }

}