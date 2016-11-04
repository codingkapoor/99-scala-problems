package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P18Spec extends FlatSpec with Matchers {

  "sliceBuiltin" should "return a list as a slice extracted from the given list" in {
    P18.sliceBuiltin(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should equal(List('d, 'e, 'f, 'g))
    P18.sliceBuiltin(3, 7, List()) should equal(List())
  }

  "sliceBuiltin" should "throw an exception for incorrect parameter values 'begin' and/or 'end'" in {
    val e = intercept[IllegalArgumentException] { P18.sliceBuiltin(3, 17, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) }
    e.getMessage.equals("Parameters 'begin' and/or 'end' are incorrect.")
  }

  "sliceBuiltinII" should "return a list as a slice extracted from the given list" in {
    P18.sliceBuiltinII(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should equal(List('d, 'e, 'f, 'g))
    P18.sliceBuiltinII(3, 7, List()) should equal(List())
  }

  "sliceBuiltinII" should "throw an exception for incorrect parameter values 'begin' and/or 'end'" in {
    val e = intercept[IllegalArgumentException] { P18.sliceBuiltinII(-1, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) }
    e.getMessage.equals("Parameters 'begin' and/or 'end' are incorrect.")
  }

  "sliceBuiltinIII" should "return a list as a slice extracted from the given list" in {
    P18.sliceBuiltinIII(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should equal(List('d, 'e, 'f, 'g))
    P18.sliceBuiltinIII(3, 7, List()) should equal(List())
  }

  "sliceBuiltinIII" should "throw an exception for incorrect parameter values 'begin' and/or 'end'" in {
    val e = intercept[IllegalArgumentException] { P18.sliceBuiltinIII(3, -2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) }
    e.getMessage.equals("Parameters 'begin' and/or 'end' are incorrect.")
  }

  "sliceFunctional" should "return a list as a slice extracted from the given list" in {
    P18.sliceFunctional(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should equal(List('d, 'e, 'f, 'g))
    P18.sliceFunctional(3, 7, List()) should equal(List())
  }

  "sliceFunctional" should "throw an exception for incorrect parameter values 'begin' and/or 'end'" in {
    val e = intercept[IllegalArgumentException] { P18.sliceFunctional(7, 3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) }
    e.getMessage.equals("Parameters 'begin' and/or 'end' are incorrect.")
  }

  "sliceFunctionalII" should "return a list as a slice extracted from the given list" in {
    P18.sliceFunctionalII(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should equal(List('d, 'e, 'f, 'g))
    P18.sliceFunctionalII(3, 7, List()) should equal(List())
  }

  "sliceFunctionalII" should "throw an exception for incorrect parameter values 'begin' and/or 'end'" in {
    val e = intercept[IllegalArgumentException] { P18.sliceFunctionalII(-1, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) }
    e.getMessage.equals("Parameters 'begin' and/or 'end' are incorrect.")
  }

  "sliceFunctionalIII" should "return a list as a slice extracted from the given list" in {
    P18.sliceFunctionalIII(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should equal(List('d, 'e, 'f, 'g))
    P18.sliceFunctionalIII(3, 7, List()) should equal(List())
  }

  "sliceFunctionalIII" should "throw an exception for incorrect parameter values 'begin' and/or 'end'" in {
    val e = intercept[IllegalArgumentException] { P18.sliceFunctionalIII(-1, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) }
    e.getMessage.equals("Parameters 'begin' and/or 'end' are incorrect.")
  }

  "sliceFunctionalIV" should "return a list as a slice extracted from the given list" in {
    P18.sliceFunctionalIV(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should equal(List('d, 'e, 'f, 'g))
    P18.sliceFunctionalIV(3, 7, List()) should equal(List())
  }

  "sliceFunctionalIV" should "throw an exception for incorrect parameter values 'begin' and/or 'end'" in {
    val e = intercept[IllegalArgumentException] { P18.sliceFunctionalIV(-1, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) }
    e.getMessage.equals("Parameters 'begin' and/or 'end' are incorrect.")
  }

  "sliceFunctionalV" should "return a list as a slice extracted from the given list" in {
    P18.sliceFunctionalV(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should equal(List('d, 'e, 'f, 'g))
    P18.sliceFunctionalV(3, 7, List()) should equal(List())
  }

  "sliceFunctionalV" should "throw an exception for incorrect parameter values 'begin' and/or 'end'" in {
    val e = intercept[IllegalArgumentException] { P18.sliceFunctionalV(-1, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) }
    e.getMessage.equals("Parameters 'begin' and/or 'end' are incorrect.")
  }

  "sliceFunctionalVI" should "return a list as a slice extracted from the given list" in {
    P18.sliceFunctionalVI(10, 10, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should equal(List())
    P18.sliceFunctionalVI(3, 7, List()) should equal(List())
  }

  "sliceFunctionalVI" should "throw an exception for incorrect parameter values 'begin' and/or 'end'" in {
    val e = intercept[IllegalArgumentException] { P18.sliceFunctionalVI(-1, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) }
    e.getMessage.equals("Parameters 'begin' and/or 'end' are incorrect.")
  }

  "sliceTailRecursive" should "return a list as a slice extracted from the given list" in {
    P18.sliceTailRecursive(0, 0, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should equal(List())
    P18.sliceTailRecursive(3, 7, List()) should equal(List())
  }

  "sliceTailRecursive" should "throw an exception for incorrect parameter values 'begin' and/or 'end'" in {
    val e = intercept[IllegalArgumentException] { P18.sliceTailRecursive(-1, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) }
    e.getMessage.equals("Parameters 'begin' and/or 'end' are incorrect.")
  }

  "sliceTailRecursiveII" should "return a list as a slice extracted from the given list" in {
    P18.sliceTailRecursiveII(0, 0, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should equal(List())
    P18.sliceTailRecursiveII(3, 7, List()) should equal(List())
  }

  "sliceTailRecursiveII" should "throw an exception for incorrect parameter values 'begin' and/or 'end'" in {
    val e = intercept[IllegalArgumentException] { P18.sliceTailRecursiveII(-1, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) }
    e.getMessage.equals("Parameters 'begin' and/or 'end' are incorrect.")
  }

}