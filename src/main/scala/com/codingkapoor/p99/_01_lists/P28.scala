package com.codingkapoor.p99._01_lists

object P28 extends App {

  def lsortBuiltin[T](ls: List[List[T]]): List[List[T]] = ls sortBy { _.length }

  def lsortBuiltinII[T](ls: List[List[T]]): List[List[T]] = ls sortWith { _.length < _.length }

  /*
   * Intermediate list using 'zip' with list length. This intermediate list is though not necessary as shown in subsequent solution.
   * 
   * scala> val xs = ls zip (ls map {_.size})
   * xs: List[(List[Symbol], Int)] = List((List('a, 'b, 'c),3), (List('d, 'e),2), (List('f, 'g, 'h),3), (List('d, 'e),2), (List('i, 'j, 'k, 'l),4), (List('m, 'n),2), (List('o),1))
   * 
   */
  def lsortFunctional[T](ls: List[List[T]]): List[List[T]] = {
    val xs = ls zip (ls map { _.size })
    (xs.tail foldLeft List(xs.head._1)) { (acc, x) => if (x._2 <= acc.head.size) x._1 :: acc else acc ::: List(x._1) }
  }

  def lsortFunctionalII[T](ls: List[List[T]]): List[List[T]] = {
    (ls.tail foldLeft List(ls.head)) { (acc, x) => if (x.size <= acc.head.size) x :: acc else acc ::: List(x) }
  }

}
