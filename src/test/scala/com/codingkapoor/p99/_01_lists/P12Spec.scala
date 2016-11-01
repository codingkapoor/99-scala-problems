package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P12Spec extends FlatSpec with Matchers {

  "decodeFunctional" should "return decoded a run-length encoded list" in {
    P12.decodeFunctional(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should equal(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    P12.decodeFunctional(Nil) should equal(Nil)
  }

  "decodeFunctionalI" should "return decoded a run-length encoded list" in {
    P12.decodeFunctionalI(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should equal(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    P12.decodeFunctionalI(Nil) should equal(Nil)
  }

  "decodeFunctionalII" should "return decoded a run-length encoded list" in {
    P12.decodeFunctionalII(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should equal(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    P12.decodeFunctionalII(Nil) should equal(Nil)
  }

  "decodeBuiltin" should "return decoded a run-length encoded list" in {
    P12.decodeBuiltin(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should equal(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    P12.decodeBuiltin(Nil) should equal(Nil)
  }

  "decodeRecursive" should "return decoded a run-length encoded list" in {
    P12.decodeRecursive(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should equal(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    P12.decodeRecursive(Nil) should equal(Nil)
  }

  "decodeTailRecursive" should "return decoded a run-length encoded list" in {
    P12.decodeTailRecursive(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should equal(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    P12.decodeTailRecursive(Nil) should equal(Nil)
  }

}