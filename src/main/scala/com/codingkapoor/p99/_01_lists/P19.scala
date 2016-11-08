package com.codingkapoor.p99._01_lists

/**
 * 
 * P19 (**) Rotate a list N places to the left.
 * Examples:
 *
 * scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 * res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
 *
 * scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 * res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
 * 
 */
object P19 {

  def rotateBuiltin[T](places: Int, ls: List[T]): List[T] = {
    val xs = ls.splitAt(if (places < 0) ls.size + places else places)
    xs._2 ::: xs._1
  }

  def rotateBuiltinII[T](places: Int, ls: List[T]): List[T] = {
    val t = if (places < 0) ls.size + places else places
    ls.drop(t) ::: ls.take(t)
  }

  def rotateTailRecursive[T](places: Int, ls: List[T]): List[T] = {

    def rotateR[T](counter: Int, result: List[T], list: List[T]): List[T] = list match {
      case Nil                  => result
      
      case xs if (counter == 0) => xs ::: result
      case xs                   => rotateR(counter - 1, result ::: List(xs.head), xs.tail)
    }

    val t = if (places < 0) ls.size + places else places
    rotateR(t, Nil, ls)
  }

}