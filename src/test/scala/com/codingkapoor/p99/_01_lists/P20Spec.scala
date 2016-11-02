package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P20Spec extends FlatSpec with Matchers {

  "removeAtBuiltin" should "return the list and the removed element in a tuple" in {
    P20.removeAtBuiltin(1, List('a, 'b, 'c, 'd)) should equal((List('a, 'c, 'd), 'b))
    P20.removeAtBuiltin(1, List()) should equal(Nil)
  }

  "removeAtBuiltinII" should "return the list and the removed element in a tuple" in {
    P20.removeAtBuiltinII(1, List("a", "b", "c", "d")) should equal((List("a", "c", "d"), "b"))
    P20.removeAtBuiltinII(1, Nil) should equal(Nil)
  }

  "removeAtFunctional" should "return the list and the removed element in a tuple" in {
    P20.removeAtFunctional(1, List('a', 'b', 'c', 'd')) should equal((List('a', 'c', 'd'), 'b'))
    P20.removeAtFunctional(1, Nil) should equal(List())
  }

  "removeAtTailRecursive" should "return the list and the removed element in a tuple" in {
    P20.removeAtTailRecursive(1, List(1, 2, 3, 4)) should equal((List(1, 3, 4), 2))
    P20.removeAtTailRecursive(1, List.empty[String]) should equal(List())
  }
}