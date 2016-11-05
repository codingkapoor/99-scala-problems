package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P28Spec extends FlatSpec with Matchers {

  "lsortBuiltin" should "return sorted list of lists according to length of sublists" in {
    P28.lsortBuiltin(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) map (_.size) should equal(List(1, 2, 2, 2, 3, 3, 4))
  }

  "lsortBuiltinII" should "return sorted list of lists according to length of sublists" in {
    P28.lsortBuiltinII(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) map (_.size) should equal(List(1, 2, 2, 2, 3, 3, 4))
  }

  "lsortFunctional" should "return sorted list of lists according to length of sublists" in {
    P28.lsortFunctional(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) map (_.size) should equal(List(1, 2, 2, 2, 3, 3, 4))
  }

  "lsortFunctionalII" should "return sorted list of lists according to length of sublists" in {
    P28.lsortFunctionalII(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) map (_.size) should equal(List(1, 2, 2, 2, 3, 3, 4))
  }

  "lsortFreqFunctional" should "return sorted list of lists according to length of sublists" in {
    P28.lsortFreqFunctional(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) map (_.size) should equal(List(4, 1, 3, 3, 2, 2, 2))
  }

  "lsortFreqFunctionalII" should "return sorted list of lists according to length of sublists" in {
    P28.lsortFreqFunctionalII(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) map (_.size) should equal(List(4, 1, 3, 3, 2, 2, 2))
  }
}