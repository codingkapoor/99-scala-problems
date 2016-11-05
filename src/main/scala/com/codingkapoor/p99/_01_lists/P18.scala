package com.codingkapoor.p99._01_lists

object P18 extends App {

  def sliceBuiltin[T](begin: Int, end: Int, ls: List[T]): List[T] = {
    if (ls == Nil) Nil
    else if (!areParametersValid(begin, end, ls.size))
      throw new IllegalArgumentException

    else ls drop (begin) take (end - begin)
  }

  def sliceBuiltinII[T](begin: Int, end: Int, ls: List[T]): List[T] = {
    if (ls == Nil) Nil
    else if (!areParametersValid(begin, end, ls.size))
      throw new IllegalArgumentException

    else ls slice (begin, end)
  }

  def sliceBuiltinIII[T](begin: Int, end: Int, ls: List[T]): List[T] = {
    if (ls == Nil) Nil
    else if (!areParametersValid(begin, end, ls.size))
      throw new IllegalArgumentException

    else ls.splitAt(begin)._2.splitAt(end - begin)._1
  }

  def sliceFunctional[T](begin: Int, end: Int, ls: List[T]): List[T] = {
    if (ls == Nil) Nil
    else if (!areParametersValid(begin, end, ls.size))
      throw new IllegalArgumentException

    else ls zip (1 to end) filter { _._2 > begin } map { _._1 }
  }

  def sliceFunctionalII[T](begin: Int, end: Int, ls: List[T]): List[T] = {
    if (ls == Nil) Nil
    else if (!areParametersValid(begin, end, ls.size))
      throw new IllegalArgumentException

    else (ls zip (1 to end) filter { _._2 > begin } unzip)._1
  }

  def sliceFunctionalIII[T](begin: Int, end: Int, ls: List[T]): List[T] = {
    if (ls == Nil) Nil
    else if (!areParametersValid(begin, end, ls.size))
      throw new IllegalArgumentException

    else for { (x, y) <- ls zip (1 to end) if y > begin } yield x
  }

  // This approach however requires complete traversal of list which is really not necessary
  def sliceFunctionalIV[T](begin: Int, end: Int, ls: List[T]): List[T] = {
    if (ls == Nil) Nil
    else if (!areParametersValid(begin, end, ls.size))
      throw new IllegalArgumentException

    else for { (x, y) <- ls.zipWithIndex if (y >= begin && y < end) } yield x
  }

  def sliceFunctionalV[T](begin: Int, end: Int, ls: List[T]): List[T] = {
    if (ls == Nil) Nil
    else if (!areParametersValid(begin, end, ls.size))
      throw new IllegalArgumentException

    else (ls zip (1 to end) foldLeft (Nil: List[T])) {
      case (acc, (x, y)) if (y > begin) => acc ::: List(x)
      case (acc, (x, y))                => acc
    }
  }

  def sliceFunctionalVI[T](begin: Int, end: Int, ls: List[T]) = {
    if (ls == Nil) Nil
    else if (!areParametersValid(begin, end, ls.size))
      throw new IllegalArgumentException

    else (ls zip (1 to end) foldRight (Nil: List[T])) {
      case ((x, y), acc) if (y > begin) => x :: acc
      case ((x, y), acc)                => acc
    }
  }

  def sliceTailRecursive[T](begin: Int, end: Int, ls: List[T]) = {

    def sliceR[T](t: Int, result: List[T], list: List[T]): List[T] = list match {
      case Nil                           => result

      case xs if (t >= begin && t < end) => sliceR(t + 1, result ::: List(xs.head), xs.tail)
      case xs                            => sliceR(t + 1, result, xs.tail)
    }

    if (ls == Nil) Nil
    else if (!areParametersValid(begin, end, ls.size))
      throw new IllegalArgumentException

    else sliceR(0, Nil, ls)
  }

  def sliceTailRecursiveII[T](begin: Int, end: Int, ls: List[T]) = {

    def sliceR[T](result: List[T], list: List[(T, Int)]): List[T] = list match {
      case (x, y) :: xs if (y >= begin && y < end) => sliceR(result ::: List(x), xs)
      case (x, y) :: xs                            => sliceR(result, xs)

      case _                                       => result
    }

    if (ls == Nil) Nil
    else if (!areParametersValid(begin, end, ls.size))
      throw new IllegalArgumentException

    else sliceR(Nil, ls.zipWithIndex)
  }

  def areParametersValid(begin: Int, end: Int, size: Int): Boolean = {
    if (begin > end || begin < 0 || end > size || end < 0) false
    else true
  }

}