package com.codingkapoor.p99._01_lists

object P15 {

  def duplicateNFunctional[T](n: Int, ls: List[T]): List[T] = ls map { x => for (i <- 1 to n) yield x } flatten

  def duplicateNFunctionalII[T](n: Int, ls: List[T]): List[T] = ls flatMap { x => for (i <- 1 to n) yield x }

  def duplicateNFunctionalIII[T](n: Int, ls: List[T]): List[T] = ls flatMap { List.fill(n)(_) }

  def duplicateNRecursive[T](n: Int, ls: List[T]): List[T] = {

    def dulplicateNR[T](ls: List[T]): List[T] = ls match {
      case Nil     => Nil
      case x :: xs => (for (i <- 1 to n) yield x).toList ::: dulplicateNR(xs)
    }

    dulplicateNR(ls)
  }

}
