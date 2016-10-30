package com.codingkapoor.p99._01_lists

object P04 {

  def lengthBuiltinI[A](ls: List[A]): Int = ls length

  def lengthBuiltinII[A](ls: List[A]): Int = ls size

  def lengthFunctionalI[A](ls: List[A]): Int = (ls foldLeft 0) { (acc, _) => acc + 1 }
  
  def lengthFunctionalII[A](ls: List[A]): Int = (ls.view foldLeft 0) { (acc, _) => acc + 1 }

  def lengthFunctionalIII[A](ls: List[A]): Int = ls map { _ => 1 } sum
  
  def lengthFunctionalIV[A](ls: List[A]): Int = ls.view map { _ => 1 } sum

  def lengthRecursive[A](ls: List[A]): Int = ls match {
    case Nil     => 0
    case _ :: xs => 1 + lengthRecursive(xs)
  }

  def lengthTailRecursive[A](ls: List[A]): Int = {

    def lengthR(result: Int, list: List[A]): Int = list match {
      case Nil     => result
      case _ :: xs => lengthR(result + 1, xs)
    }

    lengthR(0, ls)
  }
  
}