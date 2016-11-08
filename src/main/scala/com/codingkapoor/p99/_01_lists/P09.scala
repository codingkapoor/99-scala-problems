package com.codingkapoor.p99._01_lists

/**
 * 
 * P09 (**) Pack consecutive duplicates of list elements into sublists.
 * [If a list contains repeated elements they should be placed in separate sublists.]
 * 
 * Example:
 * 
 * scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
 * res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
 * 
 */
object P09 {

  // We have to handle initial value of accumulator as 'List(Nil)' to return seed value
  // because we can't invoke combinators on empty lists.
  def packFunctional[T](ls: List[T]): List[List[T]] = {

    (ls foldLeft List(List.empty[T])) {
      case (List(Nil), x)                      => List(List(x))

      case (acc, x) if (acc.flatten.last == x) => acc.init ::: List(acc.last ::: List(x))
      case (acc, x)                            => acc ::: List(List(x))
    }

  }

  // Using built-in 'takeWhile' and 'dropWhile' functions
  def packRecursive[T](ls: List[T]): List[List[T]] = ls match {
    case Nil => Nil
    case xs  => xs.takeWhile { _ == ls.head } :: packRecursive(xs.dropWhile { _ == ls.head })
  }

  def packTailRecursive[T](ls: List[T]): List[List[T]] = {

    def packR(result: List[List[T]], list: List[T]): List[List[T]] = list match {
      case Nil => result
      case xs  => packR(result ::: List(xs.takeWhile { _ == xs.head }), xs.dropWhile { _ == xs.head })
    }

    packR(List(ls.takeWhile { _ == ls.head }), ls.dropWhile { _ == ls.head })
  }

  // Using built-in 'scan' function
  def packTailRecursiveII[T](ls: List[T]): List[List[T]] = {
    
    def packR[T](result: List[List[T]], list: List[T]): List[List[T]] = list match {
      case Nil => result
      case xs => {
        val (packed, next) = xs span { _ == xs.head }
        packR(result ::: List(packed), next)
      }
    }

    val (packed, next) = ls span { _ == ls.head }
    packR(List(packed), next)
  }

}