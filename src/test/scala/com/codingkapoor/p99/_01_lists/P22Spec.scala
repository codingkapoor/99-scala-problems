package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P22Spec extends FlatSpec with Matchers {

  "rangeBuiltin" should "return a list containing all integers within a given range" in {
    P22.rangeBuiltin(4, 9) should equal(List(4, 5, 6, 7, 8, 9))
  }

  "rangeBuiltin" should "return IllegalArgumentException if parameter 'end' is lesser than parameter 'begin'" in {
    intercept[IllegalArgumentException] { P22.rangeBuiltin(9, 4) }
  }

  "rangeBuiltinII" should "return a list containing all integers within a given range" in {
    P22.rangeBuiltinII(4, 9) should equal(List(4, 5, 6, 7, 8, 9))
  }

  "rangeBuiltinII" should "return IllegalArgumentException if parameter 'end' is lesser than parameter 'begin'" in {
    intercept[IllegalArgumentException] { P22.rangeBuiltinII(9, 4) }
  }

  "rangeTailRecursive" should "return a list containing all integers within a given range" in {
    P22.rangeTailRecursive(4, 9) should equal(List(4, 5, 6, 7, 8, 9))
  }

  "rangeTailRecursive" should "return IllegalArgumentException if parameter 'end' is lesser than parameter 'begin'" in {
    intercept[IllegalArgumentException] { P22.rangeTailRecursive(9, 4) }
  }
}