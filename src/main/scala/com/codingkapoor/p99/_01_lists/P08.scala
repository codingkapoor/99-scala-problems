package com.codingkapoor.p99._01_lists

import scala.annotation.tailrec

object P08 {

  // In order to preserve the order of the elements we need to concatenate every unique subsequent element 
  // as list to the accumulator.
  def compressFunctional[A](ls: List[A]): List[A] = {
    if (ls == Nil) Nil
    else (ls foldLeft List(ls.head)) { (acc, x) => if (acc.last != x) acc ::: List(x); else acc }
  }

  // The anonymous function passed to 'foldRight' take accumulator as second argument.
  def compressFunctionalII[A](ls: List[A]): List[A] = {
    if (ls == Nil) Nil
    else (ls foldRight List(ls.last)) { (x, acc) => if (acc.head != x) x :: acc; else acc }
  }

  def compressRecursive[A](ls: List[A]): List[A] = ls match {
    case Nil       => Nil
    case h :: tail => h :: compressRecursive(tail.dropWhile(_ == h))
  }

  // Using pattern matching a list can always be traversed from left to right. 
  def compressTailRecursive[A](ls: List[A]): List[A] = {

    @tailrec
    def compressR(result: List[A], list: List[A]): List[A] = list match {

      case x :: Nil => if (x == result.last) result; else result ::: List(x)

      case x :: xs  => if (x == result.last) compressR(result, xs); else compressR(result ::: List(x), xs)

      case _        => Nil

    }

    if (ls == Nil) Nil
    else compressR(List(ls.head), ls)

  }

  // By concatenating 'h' to result as list, we can avoid reversing the result all together
  def compressTailRecursiveII[A](ls: List[A]): List[A] = {

    @tailrec
    def compressR(result: List[A], list: List[A]): List[A] = list match {
      case Nil       => result
      case h :: tail => compressR(result ::: List(h), tail.dropWhile(_ == h))
    }

    compressR(Nil, ls)
  }

}
