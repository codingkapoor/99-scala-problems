package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P21Spec extends FlatSpec with Matchers {

  "insertAtBuiltin" should "return a list with new element added to the given list at specified position" in {
    P21.insertAtBuiltin('new, 0, List('a, 'b, 'c, 'd)) should equal(List('new, 'a, 'b, 'c, 'd))
    P21.insertAtBuiltin('new, 1, List()) should equal(List())
  }

  "insertAtBuiltin" should "throw an IllegalArgumentException if position specified is less than 0" in {
    val e = intercept[IllegalArgumentException](P21.insertAtBuiltin('new, -1, List('a, 'b, 'c, 'd)))
    e.getMessage.equals("position should be greater than 0")
  }

  "insertAtBuiltinII" should "return a list with new element added to the given list at specified position" in {
    P21.insertAtBuiltinII('x', 1, List('a', 'b', 'c', 'd')) should equal(List('a', 'x', 'b', 'c', 'd'))
    P21.insertAtBuiltinII('new, 1, Nil) should equal(List())
  }

  "insertAtBuiltinII" should "throw an IllegalArgumentException if position specified is less than 0" in {
    val e = intercept[IllegalArgumentException](P21.insertAtBuiltin('new, -1, List('a, 'b, 'c, 'd)))
    e.getMessage.equals("position should be greater than 0")
  }

  "insertAtTailRecursive" should "return a list with new element added to the given list at specified position" in {
    P21.insertAtTailRecursive("new", 1, List("a", "b", "c", "d")) should equal(List("a", "new", "b", "c", "d"))
    P21.insertAtTailRecursive('new, 1, List.empty[String]) should equal(Nil)
  }

  "insertAtTailRecursive" should "throw an IllegalArgumentException if position specified is less than 0" in {
    val e = intercept[IllegalArgumentException](P21.insertAtBuiltin('new, -1, List('a, 'b, 'c, 'd)))
    e.getMessage.equals("position should be greater than 0")
  }

}