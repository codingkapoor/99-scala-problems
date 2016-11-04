package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P23Spec extends FlatSpec with Matchers {

  "randomSelectFunctional" should "return a list of extracted a given number of randomly selected elements from a list" in {
    P23.randomSelectFunctional(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)).size should equal(3)
    P23.randomSelectFunctional(3, List()) should equal(Nil)
  }

  "randomSelectTailRecursive" should "return a list of extracted a given number of randomly selected elements from a list" in {
    P23.randomSelectTailRecursive(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)).size should equal(3)
    P23.randomSelectTailRecursive(3, List()) should equal(Nil)
  }
}