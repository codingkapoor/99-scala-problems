package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P13Spec extends FlatSpec with Matchers {

  "encodeDirectRecursive" should "return run-length encoding of a list (direct solution)" in {
    P13.encodeDirectRecursive(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should equal(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    P13.encodeDirectRecursive(Nil) should equal(Nil)
  }

  "encodeDirectRecursiveII" should "return run-length encoding of a list (direct solution)" in {
    P13.encodeDirectRecursiveII(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should equal(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    P13.encodeDirectRecursiveII(Nil) should equal(Nil)
  }

}