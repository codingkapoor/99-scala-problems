package com.codingkapoor.p99._01_lists

object P05 {
  
  def reverseBuiltin[A](ls: List[A]): List[A] = ls.reverse

  def reverseFunctional[A](ls: List[A]): List[A] = ls.foldLeft(List[A]()) { (r, h) => h :: r }

  def reverseRecursive[A](ls: List[A]): List[A] = ls match {
    case Nil     => Nil
    case x :: xs => reverseRecursive(xs) ::: List(x)
  }

  // Building a left bounded sequence out of the given list
  def reverseRecursiveII[A](ls: List[A]): Seq[A] = ls match {
    case Nil      => Nil

    case y :: Nil => Nil :+ y
    case x :: xs  => reverseRecursiveII(xs) :+ x
  }

  def reverseTailRecursive[A](ls: List[A]): List[A] = {

    def reverseR(result: List[A], list: List[A]): List[A] = list match {
      case Nil     => result
      case x :: xs => reverseR(x :: result, xs)
    }

    reverseR(Nil, ls)
  }
  
}