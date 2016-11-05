package com.codingkapoor.p99._01_lists

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