package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P04Spec extends FlatSpec with Matchers {

  "lengthBuiltinI" should "return the length of the list" in {
    P04.lengthBuiltinI(List(1, 2, 3, 4)) should equal(4)
  }

  "lengthBuiltinII" should "return the length of the list" in {
    P04.lengthBuiltinI(List(1)) should equal(1)
  }

  "lengthFunctionalI" should "return the length of the list" in {
    P04.lengthBuiltinI(Nil) should equal(0)
  }

  "lengthFunctionalII" should "return the length of the list" in {
    P04.lengthBuiltinI(List('a', 'b')) should equal(2)
  }

  "lengthFunctionalIII" should "return the length of the list" in {
    P04.lengthBuiltinI(List("a")) should equal(1)
  }

  "lengthFunctionalIV" should "return the length of the list" in {
    P04.lengthBuiltinI(List()) should equal(0)
  }

  "lengthRecursiveI" should "return the length of the list" in {
    P04.lengthBuiltinI(List(1, 2, 3)) should equal(3)
  }

  "lengthRecursiveII" should "return the length of the list" in {
    P04.lengthBuiltinI(List.empty[String]) should equal(0)
  }
}