package com.codingkapoor.p99._01_lists

object P01 {

  // Using Scala's "zipWithIndex" method to build tuples of elements with their indexes 
  // and then filtering out the element with the index that is equal to the size of the list minus 1 
  def lastElement[A](ls: List[A]) = {

    val indexOfLastElement = ls.size - 1

    val Some((a, b)) = (ls zipWithIndex) filter { case (x, y) => y == `indexOfLastElement` } lift 0

    a
  }

  // Using Scala's built-in "last" method to retrieve the last element 
  def lastBuiltin[A](ls: List[A]): A = ls.last

  // Using recursive approach to ignore first element in a list up until it is the last element in that list
  def lastRecursive[A](ls: List[A]): A = ls match {
    case head :: Nil => head
    case _ :: tail   => lastRecursive(tail)
    case _           => throw new NoSuchElementException("Empty List")
  }

}

