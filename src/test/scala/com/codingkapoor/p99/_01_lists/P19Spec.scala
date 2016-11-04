package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P19Spec extends FlatSpec with Matchers {

  "rotateBuiltin" should "return a list rotated N places to the left" in {
    P19.rotateBuiltin(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should equal(List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    P19.rotateBuiltin(3, List()) should equal(List())
  }

  "rotateBuiltinII" should "return a list rotated N places to the left" in {
    P19.rotateBuiltinII(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should equal(List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
    P19.rotateBuiltinII(3, List()) should equal(Nil)
  }

  "rotateTailRecursive" should "return a list rotated N places to the left" in {
    P19.rotateTailRecursive(11, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should equal(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    P19.rotateTailRecursive(3, Nil) should equal(List())
  }

}