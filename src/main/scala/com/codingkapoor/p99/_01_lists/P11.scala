package com.codingkapoor.p99._01_lists

object P11 {

  import P10._

  def encodeModifiedFunctional[T](ls: List[T]) = {

    if (ls == Nil) Nil
    else encodeFucntional(ls) map { case (x, y) => if (x == 1) y else (x, y) }
  }

  def encodeModifiedFunctionalI[T](ls: List[T]) = {

    if (ls == Nil) Nil
    else (encodeFucntional(ls).reverse foldLeft (Nil: List[Any])) {
      (acc, x) =>
        x match {
          case (p, q) if (p == 1) => q :: acc
          case (p, q)             => (p, q) :: acc
        }
    }
  }

  def encodeModifiedRecursive[T](ls: List[T]) = {

    def encodeModifiedR[T](list: List[T]): List[Any] = list match {
      case Nil                      => Nil
      case (p, q) :: xs if (p == 1) => q :: encodeModifiedR(xs)
      case (p, q) :: xs             => (p, q) :: encodeModifiedR(xs)
    }

    encodeModifiedR(encodeFucntional(ls))
  }

  def encodeModifiedTailRecursive[T](ls: List[T]) = {

    def encodeModifiedR[T](result: List[Any], list: List[T]): List[Any] = list match {
      case Nil                      => result
      case (p, q) :: xs if (p == 1) => encodeModifiedR(result ::: List(q), xs)
      case (p, q) :: xs             => encodeModifiedR(result ::: List((p, q)), xs)
    }

    encodeModifiedR(Nil, encodeFucntional(ls))
  }

}
