package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P10Spec extends FlatSpec with Matchers {

  "encodeFucntional" should "return run-length encoding of a list" in {
    P10.encodeFunctional(List(1, 1, 1, 2, 2, 1, 3, 3, 3, 4)) should equal(List((3, 1), (2, 2), (1, 1), (3, 3), (1, 4)))
  }

  "encodeFucntionalII" should "return run-length encoding of a list" in {
    P10.encodeFunctionalII(List(1, 1, 1, 2, 2, 1, 3, 3, 3, 4)) should equal(List((3, 1), (2, 2), (1, 1), (3, 3), (1, 4)))
  }

  "encodeFucntionalIII" should "return run-length encoding of a list" in {
    P10.encodeFunctionalIII(List(1, 1, 1, 2, 2, 1, 3, 3, 3, 4)) should equal(List((3, 1), (2, 2), (1, 1), (3, 3), (1, 4)))
  }

  "encodeRecursive" should "return run-length encoding of a list" in {
    P10.encodeRecursive(List(1, 1, 1, 2, 2, 1, 3, 3, 3, 4)) should equal(List((3, 1), (2, 2), (1, 1), (3, 3), (1, 4)))
  }

}