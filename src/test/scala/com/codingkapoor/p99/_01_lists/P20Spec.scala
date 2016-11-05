package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P20Spec extends FlatSpec with Matchers {

  "removeAtBuiltin" should "return the list and the removed element in a tuple" in {
    P20.removeAtBuiltin(1, List('a, 'b, 'c, 'd)) should equal((List('a, 'c, 'd), 'b))
  }

  "removeAtBuiltin" should "throw NoSuchElementException if the list is empty" in {
    val e = intercept[NoSuchElementException] { P20.removeAtBuiltin(1, Nil) }
    e.getMessage.equals("List is empty.")
  }

  "removeAtBuiltinII" should "return the list and the removed element in a tuple" in {
    P20.removeAtBuiltinII(1, List("a", "b", "c", "d")) should equal((List("a", "c", "d"), "b"))
  }

  "removeAtBuiltinII" should "throw NoSuchElementException if the list is empty" in {
    val e = intercept[NoSuchElementException] { P20.removeAtBuiltinII(1, List()) }
    e.getMessage.equals("List is empty.")
  }

  "removeAtFunctional" should "return the list and the removed element in a tuple" in {
    P20.removeAtFunctional(1, List('a', 'b', 'c', 'd')) should equal((List('a', 'c', 'd'), 'b'))
  }

  "removeAtFunctional" should "throw NoSuchElementException if the list is empty" in {
    val e = intercept[NoSuchElementException] { P20.removeAtFunctional(1, List.empty[String]) }
    e.getMessage.equals("List is empty.")
  }

  "removeAtTailRecursive" should "return the list and the removed element in a tuple" in {
    P20.removeAtTailRecursive(1, List(1, 2, 3, 4)) should equal((List(1, 3, 4), 2))
  }

  "removeAtTailRecursive" should "throw NoSuchElementException if the list is empty" in {
    val e = intercept[NoSuchElementException] { P20.removeAtTailRecursive(1, List()) }
    e.getMessage.equals("List is empty.")
  }
}