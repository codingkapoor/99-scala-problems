package com.codingkapoor.p99._01_lists

object P21 {

  def insertAtBuiltin[T](elem: T, pos: Int, ls: List[T]) = {
    if (pos < 0) throw new IllegalArgumentException("position should be greater than 0")

    if (ls == Nil) Nil
    else ls.splitAt(pos) match { case (x, y) => x ::: elem :: y }
  }

  def insertAtBuiltinII[T](elem: T, pos: Int, ls: List[T]) = {
    if (pos < 0) throw new IllegalArgumentException("position should be greater than 0")

    if (ls == Nil) Nil
    else ls.take(pos) ::: elem :: ls.drop(pos)
  }

  // We get error 'type mismatch; found : List[Any] required: List[T]' for keeping
  // return type of 'insertAtR' as 'List[T]'. The reason still being unknown to me as of now.
  def insertAtTailRecursive[T](elem: T, pos: Int, ls: List[T]) = {

    @scala.annotation.tailrec
    def insertAtR[T](t: Int, result: List[T], list: List[T]): List[Any] = list match {
      case Nil            => Nil
      case xs if (t == 0) => result ::: List(elem) ::: xs
      case xs             => insertAtR(t - 1, result ::: List(xs.head), xs.tail)
    }

    if (pos < 0) throw new IllegalArgumentException("position should be greater than 0")

    insertAtR(pos, Nil, ls)
  }

  // foldLeft won't be an appropriate solution here since we don't really need to traverse 
  // the complete list just to insert an element at a given position.
}