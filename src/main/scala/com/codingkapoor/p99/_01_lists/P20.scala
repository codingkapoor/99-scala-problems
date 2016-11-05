package com.codingkapoor.p99._01_lists

object P20 {

  def removeAtBuiltin[T](pos: Int, ls: List[T]): (List[T], T) = ls.splitAt(pos + 1) match {
    case (Nil, Nil) => throw new NoSuchElementException("List is empty.")
    case (x, y)     => (x.init ::: y, x.last)
  }

  def removeAtBuiltinII[T](pos: Int, ls: List[T]): (List[T], T) = {
    if (ls == Nil) throw new NoSuchElementException("List is empty.")

    lazy val y = ls.take(pos + 1)
    (y.init ::: ls.drop(pos + 1), y.last)
  }

  // Build a list 'y' without the element at the specified position and then return result
  // as tuple of y and ls(pos)
  def removeAtFunctional[T](pos: Int, ls: List[T]): (List[T], T) = {

    lazy val y = (ls.zipWithIndex foldLeft (Nil: List[T])) {
      case (acc, (p, q)) if (q == pos) => acc
      case (acc, (p, q))               => acc ::: List(p)
    }

    if (ls == Nil) throw new NoSuchElementException("List is empty.")
    (y, ls(pos))
  }

  def removeAtTailRecursive[T](pos: Int, ls: List[T]): (List[T], T) = {

    @scala.annotation.tailrec
    def removeAtR[T](t: Int, result: List[T], list: List[T]): (List[T], T) = list match {
      case x :: xs if (t == 1) => (result ::: xs, x)
      case x :: xs             => removeAtR(t - 1, result ::: List(x), xs)
    }

    if (ls == Nil) throw new NoSuchElementException("List is empty.")
    removeAtR(pos + 1, Nil, ls)
  }

}
