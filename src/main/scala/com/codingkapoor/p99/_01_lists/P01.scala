package com.codingkapoor.p99._01_lists

object P01 extends App {

  // Using Scala's "zipWithIndex" method to build tuples of elements with their indexes 
  // and then filtering out the element with the index that is equal to the size of the list minus 1 
  def lastElement[A](ls: List[A]) = {

    if (ls == Nil) throw new NoSuchElementException("List is empty.")

    val List((a, b)) = (ls zipWithIndex) filter { case (x, y) => y == (ls.size - 1) }

    a
  }

  def lastElementII[A](ls: List[A]): A = {

    val x = (ls lift ls.size - 1) match {
      case Some(y) => y
      case None    => throw new NoSuchElementException
    }

    x
  }

  def lastBuiltin[A](ls: List[A]): A = ls.last

  // Using recursive approach to ignore first element in a list up until it is the last element in that list
  def lastRecursive[A](ls: List[A]): A = ls match {

    case Nil         => throw new NoSuchElementException("List is empty.")

    case head :: Nil => head
    case _ :: tail   => lastRecursive(tail)

  }

  println(lastElement(List()))

}

