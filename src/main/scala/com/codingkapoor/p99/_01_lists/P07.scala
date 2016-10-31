package com.codingkapoor.p99._01_lists

object P07 {
  
  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case xs: List[_] => flatten(xs)
    case y           => List(y)
  }
  
}