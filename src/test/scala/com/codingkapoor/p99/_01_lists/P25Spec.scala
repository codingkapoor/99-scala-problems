package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P25Spec extends FlatSpec with Matchers {

  "randomPermute" should "return a random permutation of the elements of a list" in {
    P25.randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)).size should equal(6)
    P25.randomPermute(List()) should equal(Nil)
  }

  "randomPermuteBuiltin" should "return a random permutation of the elements of a list" in {
    P25.randomPermuteBuiltin(List('a, 'b, 'c, 'd, 'e, 'f)).size should equal(6)
    P25.randomPermuteBuiltin(List()) should equal(Nil)
  }

  "randomPermuteBuiltinII" should "return a random permutation of the elements of a list" in {
    P25.randomPermuteBuiltinII(List('a, 'b, 'c, 'd, 'e, 'f)).size should equal(6)
    P25.randomPermuteBuiltinII(List()) should equal(Nil)
  }
}