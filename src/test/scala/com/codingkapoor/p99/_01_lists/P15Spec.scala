package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P15Spec extends FlatSpec with Matchers {

  "duplicateNFunctional" should "return a list with elements in a given list duplicated the given number of times" in {
    P15.duplicateNFunctional(3, List('a, 'b, 'c, 'c, 'd)) should equal(List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
    P15.duplicateNFunctional(3, Nil) should equal(Nil)
  }

  "duplicateNFunctionalII" should "return a list with elements in a given list duplicated the given number of times" in {
    P15.duplicateNFunctionalII(1, List(1, 2, 3, 3, 4)) should equal(List(1, 2, 3, 3, 4))
    P15.duplicateNFunctionalII(3, List()) should equal(Nil)
  }

  "duplicateNFunctionalIII" should "return a list with elements in a given list duplicated the given number of times" in {
    P15.duplicateNFunctionalIII(3, List("a", "b", "c", "c", "d")) should equal(List("a", "a", "a", "b", "b", "b", "c", "c", "c", "c", "c", "c", "d", "d", "d"))
    P15.duplicateNFunctionalIII(3, List()) should equal(List())
  }

  "duplicateNRecursive" should "return a list with elements in a given list duplicated the given number of times" in {
    P15.duplicateNRecursive(3, List('a, 'b, 'c, 'c, 'd)) should equal(List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
    P15.duplicateNRecursive(3, List.empty[Int]) should equal(Nil)
  }
}