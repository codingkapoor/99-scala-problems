package com.codingkapoor.p99._01_lists

/**
 * 
 * P28 (**) Sorting a list of lists according to length of sublists.
 * a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of the list according to their length. 
 * E.g. short lists first, longer lists later, or vice versa.
 *
 * Example:
 *
 * scala> lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
 * res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
 *
 * b) Again, we suppose that a list contains elements that are lists themselves. 
 * But this time the objective is to sort the elements according to their length frequency; 
 * i.e. in the default, sorting is done ascendingly, lists with rare lengths are placed, others with a more frequent length come later.
 *
 * Example:
 *
 * scala> lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
 * res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
 *
 * [Note that in the above example, the first two lists in the result have length 4 and 1 and both lengths appear just once. 
 * The third and fourth lists have length 3 and there are two list of this length. Finally, the last three lists have length 2. This is the most frequent length.]
 * 
 */
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
