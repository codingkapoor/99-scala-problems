package com.codingkapoor.p99._01_lists

object P28 {

  def lsortBuiltin[T](ls: List[List[T]]): List[List[T]] = ls sortBy { _.length }

  def lsortBuiltinII[T](ls: List[List[T]]): List[List[T]] = ls sortWith { _.length < _.length }

  /*
   * Intermediate list using 'zip' with length of lists. This intermediate list is not necessary as shown in subsequent solution.
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

  def lsortFreqFunctional[T](ls: List[List[T]]): List[List[T]] =
    (ls sortBy { _.length } groupBy { _.length } values).toList sortBy { _.length } flatten

  /*
   * Intermediate list using 'zip' with frequency of length of lists.
   * 
   * scala> ls zip (k map {x => k.count(_ == x)})
	 * res151: List[(List[Symbol], Int)] = List((List('a, 'b, 'c),2), (List('d, 'e),3), (List('f, 'g, 'h),2), (List('d, 'e),3), (List('i, 'j, 'k, 'l),1), (List('m, 'n),3), (List('o),1))*
	 * 
	 * Then sorting list of lists by count of list size. 
   */
  def lsortFreqFunctionalII[T](ls: List[List[T]]): List[List[T]] = {
    val k = ls map { _.size }
    ls zip (k map { x => k.count(_ == x) }) sortBy { _._2 } map { _._1 }
  }

}
