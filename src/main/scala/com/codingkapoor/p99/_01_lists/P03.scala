package com.codingkapoor.p99._01_lists

object P03 {

  // "position" refers to the position of the element in the list and not it's index
  def nthElement[A](position: Int, ls: List[A]) = {

    if (ls == Nil) throw new NoSuchElementException("List is empty.")

    val List((a, b)) = (ls zipWithIndex) filter {
      case (x, y) => if (position > ls.size) throw new NoSuchElementException else y == (position - 1)
    }

    a
  }

  def nthElementII[A](position: Int, ls: List[A]): A = {

    val x = (ls lift position - 1) match {
      case Some(y) => y
      case None    => throw new NoSuchElementException
    }

    x
  }

  def nthBuiltin[A](position: Int, ls: List[A]): A = ls take (position) last

  def nthBuiltinII[A](position: Int, ls: List[A]): A = {
    if (position > ls.size || position <= 0) throw new NoSuchElementException
    ls(position - 1)
  }

  def nthRecursive[A](position: Int, ls: List[A]): A = (position, ls) match {

    case (_, Nil)            => throw new NoSuchElementException("List is empty.")
    case (_, _ :: Nil)       => throw new NoSuchElementException

    case (1, x :: _)         => x
    case (position, _ :: xs) => nthRecursive(position - 1, xs)

  }

}