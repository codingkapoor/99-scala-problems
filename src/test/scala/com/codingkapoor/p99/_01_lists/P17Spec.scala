package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P17Spec extends FlatSpec with Matchers {

  "splitBuiltin" should "return a tuple that holds splitted part of a list as it's elements" in {
    P17.splitBuiltin(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should equal((List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    P17.splitBuiltin(3, Nil) should equal((List(), List()))
  }

  "splitBuiltinII" should "return a tuple that holds splitted part of a list as it's elements" in {
    P17.splitBuiltinII(3, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)) should equal((List(1, 2, 3), List(4, 5, 6, 7, 8, 9, 10, 11)))
    P17.splitBuiltinII(3, List.empty[String]) should equal((List(), List()))
  }

  "splitFunctional" should "return a tuple that holds splitted part of a list as it's elements" in {
    P17.splitFunctional(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')) should equal((List('a', 'b', 'c'), List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
    P17.splitFunctional(3, List()) should equal((List(), List()))
  }

  "splitTailRecursive" should "return a tuple that holds splitted part of a list as it's elements" in {
    P17.splitTailRecursive(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should equal((List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    P17.splitTailRecursive(3, Nil) should equal((List(), List()))
  }
}