package com.codingkapoor.p99._01_lists

object P22 {

  def rangeBuiltin(begin: Int, end: Int) = {
    if (end < begin) throw new IllegalArgumentException("Parameter 'end' has to be greater than or equal to parameter 'begin'")
    List.range(begin, end + 1)
  }

  def rangeBuiltinII(begin: Int, end: Int) = {
    if (end < begin) throw new IllegalArgumentException("Parameter 'end' has to be greater than or equal to parameter 'begin'")
    List.fill(end - begin)(1).scan(begin)(_ + _)
  }

  def rangeTailRecursive(begin: Int, end: Int) = {
    if (end < begin) throw new IllegalArgumentException("Parameter 'end' has to be greater than or equal to parameter 'begin'")
    
    def rangeR(t: Int, result: List[Int]): List[Int] = {
      if (t == end + 1) result
      else rangeR(t + 1, result ::: List(t))
    }

    rangeR(begin, Nil)
  }

}