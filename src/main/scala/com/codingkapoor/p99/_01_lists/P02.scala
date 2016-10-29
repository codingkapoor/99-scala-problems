package com.codingkapoor.p99._01_lists

object P02 {

  // Using Scala's "zipWithIndex" method to build tuples of elements with their indexes 
  // and then filtering out the element with the index that is equal to the size of the list minus 2
  def secondLastElement[A](ls: List[A]) = {

    val indexOfSecondLastElement = ls.size - 2

    val Some((a, b)) = (ls zipWithIndex) filter { case (x, y) => y == `indexOfSecondLastElement` } lift 0

    a
  }

  // Using Scala's built-in "init" method to retrieve subset of the specified list with all elements  
  // less last element and then using "last" method to retrieve the last element of the sub list 
  def secondLastBuiltin[A](ls: List[A]): A =
    if (ls.isEmpty) throw new NoSuchElementException("List is empty.")
    else ls.init.last

  // Using recursive approach to ignore first element in a list up until it is the second last element in that list
  def secondLastRecursive[A](ls: List[A]): A = ls match {

    case Nil           => throw new NoSuchElementException("List is empty.")
    case x :: Nil      => throw new NoSuchElementException("Second last element can't be found. List has only 1 element.")

    case x :: _ :: Nil => x
    case _ :: xs       => secondLastRecursive(xs)

  }

}
