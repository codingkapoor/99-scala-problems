package com.codingkapoor.p99._01_lists

/**
 * 
 * P14 (*) Duplicate the elements of a list.
 * Example:
 *
 * scala> duplicate(List('a, 'b, 'c, 'c, 'd))
 * res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
 * 
 */
object P14 {

  def duplicateFunctional[T](ls: List[T]): List[T] = ls map { x => List(x, x) } flatten

  def duplicateFunctionalII[T](ls: List[T]): List[T] = ls flatMap { x => List(x, x) }

  def duplicateFunctionalIII[T](ls: List[T]): List[T] =
    // 'reverse' function has complexity O(n) next to regular traversal. Hence should be avoided 
    // (ls.reverse foldLeft (Nil: List[T])) { (acc, x) => x :: x :: acc }
    (ls foldLeft (Nil: List[T])) { (acc, x) => acc ::: List(x, x) }

  def duplicateRecursive[T](ls: List[T]): List[T] = ls match {
    case Nil     => Nil
    case x :: xs => x :: x :: duplicateRecursive(xs)
  }

  def duplicateTailRecursive[T](ls: List[T]): List[T] = {

    def duplicateR[T](result: List[T], list: List[T]): List[T] = list match {
      case Nil     => result
      case x :: xs => duplicateR(result ::: List(x, x), xs)
    }

    duplicateR(Nil, ls)
  }

}
