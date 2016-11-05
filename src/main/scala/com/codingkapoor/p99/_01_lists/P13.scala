package com.codingkapoor.p99._01_lists

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
