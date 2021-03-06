package com.codingkapoor.p99._01_lists

/**
 * 
 * P16 (**) Drop every Nth element from a list.
 * Example:
 *
 * scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 * res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
 * 
 */
object P16 {

  def dropFunctional[T](n: Int, ls: List[T]): List[T] =
    // ls.zipWithIndex filter { case (x, y) => (y + 1) % n != 0 } map { case (x, y) => x }
    ls.zipWithIndex.withFilter { case (x, y) => (y + 1) % n != 0 } map { _._1 }

  def dropFunctionalII[T](n: Int, ls: List[T]): List[T] =
    (ls.zipWithIndex foldLeft (Nil: List[T])) {
      case (acc, (p, q)) if ((q + 1) % n != 0) => acc ::: List(p)
      case (acc, (p, q))                       => acc
    }

  def dropRecursive[T](n: Int, ls: List[T]): List[T] = {

    def dropR[T](list: List[T]): List[T] = list.splitAt(n) match {
      case (xs, Nil) if (xs.length < n) => xs
      case (xs, Nil)                    => xs.init
      case (xs, ys)                     => xs.init ::: dropR(ys)
    }

    dropR(ls)
  }

  def dropRecursiveII[T](n: Int, ls: List[T]): List[T] = {

    def dropR[T](list: List[T]): List[T] = list match {
      case Nil                   => Nil

      case xs if (xs.length < n) => xs.take(n) ::: dropR(xs.drop(n))
      case xs                    => xs.take(n).init ::: dropR(xs.drop(n))
    }

    dropR(ls)
  }

  def dropTailRecursive[T](n: Int, ls: List[T]): List[T] = {

    def dropR[T](t: Int, result: List[T], list: List[T]): List[T] = list match {
      case Nil                 => result
      case x :: xs if (t == 1) => dropR(n, result, xs)
      case x :: xs             => dropR(t - 1, result ::: List(x), xs)
    }

    dropR(n, Nil, ls)
  }

}
