package com.codingkapoor.p99._01_lists

object P21 {

  def insertAtBuiltin[T](elem: T, pos: Int, ls: List[T]): List[T] = {
    if (pos < 0) throw new IllegalArgumentException("position should be greater than 0")

    if (ls == Nil) Nil
    else ls.splitAt(pos) match { case (x, y) => x ::: elem :: y }
  }

  def insertAtBuiltinII[T](elem: T, pos: Int, ls: List[T]): List[T] = {
    if (pos < 0) throw new IllegalArgumentException("position should be greater than 0")

    if (ls == Nil) Nil
    else ls.take(pos) ::: elem :: ls.drop(pos)
  }

  // We get error 'type mismatch; found : List[Any] required: List[T]' for keeping
  // return type of 'insertAtR' as 'List[T]'. The reason still being unknown to me as of now.
  
  // This was happening because I was defining method 'insertAtR' with the type parameter similar
  // to the type parameter of its enclosing method 'insertAtTailRecursive'. I still need to 
  // understand the reason behind it.
  def insertAtTailRecursive[T](elem: T, pos: Int, ls: List[T]): List[T] = {

    @scala.annotation.tailrec
    def insertAtR(t: Int, result: List[T], list: List[T]): List[T] = list match {
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