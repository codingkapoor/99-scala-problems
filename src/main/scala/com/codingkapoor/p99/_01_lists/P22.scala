package com.codingkapoor.p99._01_lists

object P22 {

  def rangeBuiltin(begin: Int, end: Int): List[Int] = {
    if (end < begin) throw new IllegalArgumentException
    List.range(begin, end + 1)
  }

  def rangeBuiltinII(begin: Int, end: Int): List[Int] = {
    if (end < begin) throw new IllegalArgumentException
    List.fill(end - begin)(1).scan(begin)(_ + _)
  }

  def rangeTailRecursive(begin: Int, end: Int): List[Int] = {
    if (end < begin) throw new IllegalArgumentException

    def rangeR(t: Int, result: List[Int]): List[Int] = {
      if (t == end + 1) result
      else rangeR(t + 1, result ::: List(t))
    }

    rangeR(begin, Nil)
  }

}