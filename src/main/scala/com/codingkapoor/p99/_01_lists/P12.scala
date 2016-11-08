package com.codingkapoor.p99._01_lists

/**
 * 
 * P12 (**) Decode a run-length encoded list.
 * [Given a run-length code list generated as specified in problem P10, construct its uncompressed version.]
 *
 * Example:
 *
 * scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
 * res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
 * 
 */
object P12 {

  def decodeFunctional[T](ls: List[(Int, T)]): List[T] = {
    (ls foldLeft (Nil: List[T])) {
      case (acc, (p, q)) => {
        val j = for (i <- 1 to p) yield q
        acc ::: j.toList
      }

      case _ => Nil
    }
  }

  def decodeFunctionalI[T](ls: List[(Int, T)]): List[T] = ls map {
    case (x, y) => for (i <- 1 to x) yield y
  } flatten

  def decodeFunctionalII[T](ls: List[(Int, T)]): List[T] = ls flatMap {
    case (x, y) => for (i <- 1 to x) yield y
  }

  def decodeBuiltin[T](ls: List[(Int, T)]): List[T] = ls flatMap {
    case (p, q) => List.fill(p)(q)
  }

  def decodeRecursive[T](ls: List[(Int, T)]): List[T] = ls match {
    case Nil          => Nil
    case (p, q) :: xs => List.fill(p)(q) ::: decodeRecursive(xs)
  }

  def decodeTailRecursive[T](ls: List[(Int, T)]): List[T] = {

    def decodeR[T](result: List[T], list: List[(Int, T)]): List[T] = list match {
      case Nil          => result
      case (p, q) :: xs => decodeR(result ::: List.fill(p)(q), xs)
    }

    decodeR(Nil, ls)
  }

}
