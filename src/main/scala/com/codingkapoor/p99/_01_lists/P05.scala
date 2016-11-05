package com.codingkapoor.p99._01_lists

object P05 {
  
  def reverseBuiltin[T](ls: List[T]): List[T] = ls.reverse

  def reverseFunctional[T](ls: List[T]): List[T] = ls.foldLeft(List[T]()) { (r, h) => h :: r }

  def reverseRecursive[T](ls: List[T]): List[T] = ls match {
    case Nil     => Nil
    case x :: xs => reverseRecursive(xs) ::: List(x)
  }

  // Building a left bounded sequence out of the given list
  def reverseRecursiveII[T](ls: List[T]): List[T] = ls match {
    case Nil      => Nil
    
    case y :: Nil => (Nil :+ y).toList
    case x :: xs  => reverseRecursiveII(xs) :+ x
  }

  def reverseTailRecursive[T](ls: List[T]): List[T] = {

    def reverseR(result: List[T], list: List[T]): List[T] = list match {
      case Nil     => result
      case x :: xs => reverseR(x :: result, xs)
    }

    reverseR(Nil, ls)
  }
  
}