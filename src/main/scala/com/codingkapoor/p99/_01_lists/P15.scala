package com.codingkapoor.p99._01_lists

/**
 *
 * P15 (**) Duplicate the elements of a list a given number of times.
 * Example:
 *
 * scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
 * res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
 * 
 */
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
