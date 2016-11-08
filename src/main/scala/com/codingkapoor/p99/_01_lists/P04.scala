package com.codingkapoor.p99._01_lists

/**
 * 
 * P04 (*) Find the number of elements of a list.
 * Example:
 *
 * scala> length(List(1, 1, 2, 3, 5, 8))
 * res0: Int = 6
 * 
 */
object P04 {

  def lengthBuiltinI[T](ls: List[T]): Int = ls length

  def lengthBuiltinII[T](ls: List[T]): Int = ls size

  def lengthFunctionalI[T](ls: List[T]): Int = (ls foldLeft 0) { (acc, _) => acc + 1 }
  
  def lengthFunctionalII[T](ls: List[T]): Int = (ls.view foldLeft 0) { (acc, _) => acc + 1 }

  def lengthFunctionalIII[T](ls: List[T]): Int = ls map { _ => 1 } sum
  
  def lengthFunctionalIV[T](ls: List[T]): Int = ls.view map { _ => 1 } sum

  def lengthRecursive[T](ls: List[T]): Int = ls match {
    case Nil     => 0
    case _ :: xs => 1 + lengthRecursive(xs)
  }

  def lengthTailRecursive[T](ls: List[T]): Int = {

    def lengthR(result: Int, list: List[T]): Int = list match {
      case Nil     => result
      case _ :: xs => lengthR(result + 1, xs)
    }

    lengthR(0, ls)
  }
  
}