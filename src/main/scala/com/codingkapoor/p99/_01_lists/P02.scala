package com.codingkapoor.p99._01_lists

object P02 {

  // Using Scala's "zipWithIndex" method to build tuples of elements with their indexes 
  // and then filtering out the element with the index that is equal to the size of the list minus 2
  def secondLastElement[A](ls: List[A]) = {

    if (ls == Nil)
      throw new NoSuchElementException("List is empty.")

    val List((a, b)) = (ls zipWithIndex) filter { case (x, y) => y == (ls.size - 2) }

    a
  }

  def secondLastElementII[A](ls: List[A]): A = {

    val x = (ls lift ls.size - 2) match {
      case Some(y) => y
      case None    => throw new NoSuchElementException
    }

    x
  }

  def secondLastBuiltin[A](ls: List[A]): A =
    if (ls.isEmpty) throw new NoSuchElementException("List is empty.")
    else ls.init.last

  def secondLastBuiltinII[A](ls: List[A]): A = ls take (ls.size - 1) last

  def secondLastRecursive[A](ls: List[A]): A = ls match {

    case Nil           => throw new NoSuchElementException("List is empty.")
    case x :: Nil      => throw new NoSuchElementException("Second last element can't be found. List has only 1 element.")

    case x :: _ :: Nil => x
    case _ :: xs       => secondLastRecursive(xs)

  }

}