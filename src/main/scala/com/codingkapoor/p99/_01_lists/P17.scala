package com.codingkapoor.p99._01_lists

object P17 {

  def splitBuiltin[T](n: Int, ls: List[T]) = ls.splitAt(n)

  def splitBuiltinII[T](n: Int, ls: List[T]) = (ls.take(n), ls.drop(n))

  def splitFunctional[T](n: Int, ls: List[T]) =
    (ls.zipWithIndex foldLeft ((Nil, Nil): (List[T], List[T]))) {
      case ((p, q), xs) =>
        xs match {
          case (x, y) if (y <= n - 1) => (p ::: List(x), q)
          case (x, y)                 => (p, q ::: List(x))
        }
    }

  // Variable 'result' builds a list that would contribute to the first tuple 
  // element of the expected output
  def splitTailRecursive[T](n: Int, ls: List[T]): (List[T], List[T]) = {

    def splitR[T](t: Int, result: List[T], ls: List[T]): (List[T], List[T]) = ls match {
      case Nil                 => (Nil, Nil)
      
      case x :: xs if (t == 1) => (result ::: List(x), xs)
      case x :: xs             => splitR((t - 1), (result ::: List(x)), xs)
    }

    splitR(n, Nil, ls)
  }

}

