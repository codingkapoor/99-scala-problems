package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P11Spec extends FlatSpec with Matchers {

  "encodeModifiedFunctional" should "return modified run-length encoding of a list" in {
    P11.encodeModifiedFunctional(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should equal(List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
    P11.encodeModifiedFunctional(Nil) should equal(Nil)
  }

  "encodeModifiedFunctionalI" should "return modified run-length encoding of a list" in {
    P11.encodeModifiedFunctionalI(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should equal(List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
    P11.encodeModifiedFunctional(Nil) should equal(Nil)
  }

  "encodeModifiedRecursive" should "return modified run-length encoding of a list" in {
    P11.encodeModifiedRecursive(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should equal(List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
    P11.encodeModifiedFunctional(Nil) should equal(Nil)
  }

  "encodeModifiedTailRecursive" should "return modified run-length encoding of a list" in {
    P11.encodeModifiedTailRecursive(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should equal(List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
    P11.encodeModifiedFunctional(Nil) should equal(Nil)
  }
}