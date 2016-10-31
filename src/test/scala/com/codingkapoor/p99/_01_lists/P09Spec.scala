package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P09Spec extends FlatSpec with Matchers {

  "packFunctional" should "return consecutive duplicate elements in a list packed into sublists." in {
    P09.packFunctional(List(1, 1, 1, 2, 2, 1, 3, 3, 3, 4)) should equal(List(List(1, 1, 1), List(2, 2), List(1), List(3, 3, 3), List(4)))
  }

  "packRecursive" should "return consecutive duplicate elements in a list packed into sublists." in {
    P09.packRecursive(List(1, 1, 1, 2, 2, 1, 3, 3, 3, 4)) should equal(List(List(1, 1, 1), List(2, 2), List(1), List(3, 3, 3), List(4)))
  }

  "packTailRecursive" should "return consecutive duplicate elements in a list packed into sublists." in {
    P09.packTailRecursive(List(1, 1, 1, 2, 2, 1, 3, 3, 3, 4)) should equal(List(List(1, 1, 1), List(2, 2), List(1), List(3, 3, 3), List(4)))
  }

  "packTailRecursiveII" should "return consecutive duplicate elements in a list packed into sublists." in {
    P09.packTailRecursiveII(List(1, 1, 1, 2, 2, 1, 3, 3, 3, 4)) should equal(List(List(1, 1, 1), List(2, 2), List(1), List(3, 3, 3), List(4)))
  }
}