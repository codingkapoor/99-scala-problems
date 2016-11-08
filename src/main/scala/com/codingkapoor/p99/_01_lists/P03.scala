package com.codingkapoor.p99._01_lists

/**
 * 
 * P03 (*) Find the Kth element of a list.
 * [By convention, the first element in the list is element 0.]
 *
 * Example:
 *
 * scala> nth(2, List(1, 1, 2, 3, 5, 8))
 * res0: Int = 2
 * 
 */
object P03 {

  // Parameter "position" refers to the position of the element in the list and not it's index
  def nthElement[T](position: Int, ls: List[T]): T = {
    if (ls == Nil) throw new NoSuchElementException("List is empty.")

    val List((a, b)) = (ls zipWithIndex) filter {
      case (x, y) => if (position > ls.size) throw new NoSuchElementException else y == (position - 1)
    }
    a
  }

  def nthElementII[T](position: Int, ls: List[T]): T = {

    val x = (ls lift position - 1) match {
      case Some(y) => y
      case None    => throw new NoSuchElementException
    }

    x
  }

  def nthBuiltin[T](position: Int, ls: List[T]): T = {
    if (position > ls.size) throw new NoSuchElementException
    ls take (position) last
  }

  def nthBuiltinII[T](position: Int, ls: List[T]): T = {
    if (position > ls.size || position <= 0) throw new NoSuchElementException
    ls(position - 1)
  }

  // Order of cases are important here. If '(_, _ :: Nil)' comes before '(1, x :: _)' 
  // Getting 1st element from a list of size 1 would always throw NoSuchElementException.
  def nthRecursive[T](position: Int, ls: List[T]): T = (position, ls) match {
    case (_, Nil)            => throw new NoSuchElementException("List is empty.")
    case (1, x :: _)         => x
    
    case (_, _ :: Nil)       => throw new NoSuchElementException
    case (position, _ :: xs) => nthRecursive(position - 1, xs)
  }

}