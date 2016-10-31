package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P07Spec extends FlatSpec with Matchers {

  "flatten" should "return a flattened List" in {
    P07.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should equal (List (1, 1, 2, 3, 5, 8))
  }
}