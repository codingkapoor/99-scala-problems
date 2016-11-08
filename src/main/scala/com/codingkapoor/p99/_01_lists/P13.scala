package com.codingkapoor.p99._01_lists

/**
 * 
 * P13 (**) Run-length encoding of a list (direct solution).
 * [Implement the so-called run-length encoding data compression method directly. 
 * I.e. don't use other methods you've written (like P09's pack); do all the work directly.]
 *
 * Example:
 *
 * scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
 * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
 * 
 */
object P13 {

  def encodeDirectRecursive[T](ls: List[T]): List[(Int, T)] = {

    def encodeDirectR[T](list: List[T]): List[(Int, T)] = list match {
      case Nil => Nil
      case xs  => (xs.takeWhile { _ == xs.head } size, xs.head) :: encodeDirectR(xs.dropWhile { _ == xs.head })
    }

    encodeDirectR(ls)
  }

  def encodeDirectRecursiveII[T](ls: List[T]): List[(Int, T)] = {

    def encodeDirectR[T](list: List[T]): List[(Int, T)] = list match {
      case Nil => Nil
      case xs  => {
        val (packed, next) = xs.span { _ == xs.head }
        (packed size, xs.head) :: encodeDirectR(next)
      }
    }

    encodeDirectR(ls)
  }

}
