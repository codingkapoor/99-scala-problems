package com.codingkapoor.p99._01_lists

/**
 * 
 * P07 (**) Flatten a nested list structure.
 * Example:
 *
 * scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
 * res0: List[Any] = List(1, 1, 2, 3, 5, 8)
 * 
 */
object P07 {
  
  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case xs: List[_] => flatten(xs)
    case y           => List(y)
  }
  
}