package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P05Spec extends FlatSpec with Matchers {

  "reverseBuiltin" should "return reverse of a list" in {
    P05.reverseBuiltin(List(1, 2, 3, 4, 5)) should equal(List(5, 4, 3, 2, 1))
  }

  "reverseFunctional" should "return reverse of a list" in {
    P05.reverseFunctional(List("X", "Y", "Z")) should equal(List("Z", "Y", "X"))
  }

  "reverseRecursive" should "return reverse of a list" in {
    P05.reverseRecursive(List('a', 'b', 'c')) should equal(List('c', 'b', 'a'))
  }

  "reverseRecursiveII" should "return reverse of a list" in {
    P05.reverseRecursiveII(List()) should equal(List())
  }

  "reverseTailRecursive" should "return reverse of a list" in {
    P05.reverseTailRecursive(Nil) should equal(Nil)
  }
}