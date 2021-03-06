package com.codingkapoor.p99._01_lists

/**
 * 
 * P08 (**) Eliminate consecutive duplicates of list elements.
 * [If a list contains repeated elements they should be replaced with a single copy of the element. 
 * The order of the elements should not be changed.]
 *
 * Example:
 *
 * scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
 * res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
 * 
 */
object P08 {

  // In order to preserve the order of the elements we need to concatenate every unique subsequent element 
  // as list to the accumulator.
  def compressFunctional[T](ls: List[T]): List[T] = {
    if (ls == Nil) Nil
    else (ls foldLeft List(ls.head)) { (acc, x) => if (acc.last != x) acc ::: List(x); else acc }
  }

  // The anonymous function passed to 'foldRight' take accumulator as second argument.
  def compressFunctionalII[T](ls: List[T]): List[T] = {
    if (ls == Nil) Nil
    else (ls foldRight List(ls.last)) { (x, acc) => if (acc.head != x) x :: acc; else acc }
  }

  def compressRecursive[T](ls: List[T]): List[T] = ls match {
    case Nil       => Nil
    case h :: tail => h :: compressRecursive(tail.dropWhile(_ == h))
  }

  
  import scala.annotation.tailrec
  
  // Using pattern matching a list can always be traversed from left to right. 
  def compressTailRecursive[T](ls: List[T]): List[T] = {

    @tailrec
    def compressR(result: List[T], list: List[T]): List[T] = list match {
      case x :: Nil => if (x == result.last) result; else result ::: List(x)
      case x :: xs  => if (x == result.last) compressR(result, xs); else compressR(result ::: List(x), xs)
      case _        => Nil
    }

    if (ls == Nil) Nil
    else compressR(List(ls.head), ls)

  }

  // By concatenating 'h' to result as list, we can avoid reversing the result all together
  def compressTailRecursiveII[T](ls: List[T]): List[T] = {

    @tailrec
    def compressR(result: List[T], list: List[T]): List[T] = list match {
      case Nil       => result
      case h :: tail => compressR(result ::: List(h), tail.dropWhile(_ == h))
    }

    compressR(Nil, ls)
  }

}
