package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P16Spec extends FlatSpec with Matchers {

  "dropFunctional" should "return a list with every nth element dropped from a given list" in {
    P16.dropFunctional(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should equal(List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
    P16.dropFunctional(3, Nil) should equal(Nil)
  }

  "dropFunctionalII" should "return a list with every nth element dropped from a given list" in {
    P16.dropFunctionalII(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')) should equal(List('a', 'b', 'd', 'e', 'g', 'h', 'j', 'k'))
    P16.dropFunctionalII(3, List()) should equal(Nil)
  }

  "dropRecursive" should "return a list with every nth element dropped from a given list" in {
    P16.dropRecursive(3, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)) should equal(List(1, 2, 4, 5, 7, 8, 10, 11))
    P16.dropRecursive(3, List.empty[String]) should equal(Nil)
  }

  "dropRecursiveII" should "return a list with every nth element dropped from a given list" in {
    P16.dropRecursiveII(3, List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")) should equal(List("a", "b", "d", "e", "g", "h", "j", "k"))
    P16.dropRecursiveII(3, Nil) should equal(List())
  }

  "dropTailRecursive" should "return a list with every nth element dropped from a given list" in {
    P16.dropTailRecursive(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should equal(List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
    P16.dropTailRecursive(3, Nil) should equal(Nil)
  }

}