package com.codingkapoor.p99._01_lists

object P20 extends App {

  def removeAtBuiltin[T](pos: Int, ls: List[T]) = ls.splitAt(pos + 1) match {
    case (Nil, Nil) => Nil
    case (x, y)     => (x.init ::: y, x.last)
  }

  def removeAtBuiltinII[T](pos: Int, ls: List[T]) = {
    lazy val y = ls.take(pos + 1)
    if (ls == Nil) Nil else (y.init ::: ls.drop(pos + 1), y.last)
  }

  // Build a list 'y' without the element at the specified position and then return result
  // as tuple of y and ls(pos)
  def removeAtFunctional[T](pos: Int, ls: List[T]) = {

    lazy val y = (ls.zipWithIndex foldLeft (Nil: List[T])) {
      (acc, x) =>
        x match {
          case (p, q) if (q == pos) => acc
          case (p, q)               => acc ::: List(p)
        }
    }

    if (ls == Nil) Nil
    else (y, ls(pos))
  }

  def removeAtTailRecursive[T](pos: Int, ls: List[T]) = {

    @scala.annotation.tailrec
    def removeAtR[T](t: Int, result: List[T], list: List[T]): (List[T], T) = list match {
      case x :: xs if (t == 1) => (result ::: xs, x)
      case x :: xs             => removeAtR(t - 1, result ::: List(x), xs)
    }

    if (ls == Nil) Nil
    else removeAtR(pos + 1, Nil, ls)
  }

  println(removeAtBuiltinII(1, List('a, 'b, 'c, 'd)))

}
