package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P14Spec extends FlatSpec with Matchers {

  "duplicateFunctional" should "return a list with elements duplicated in specified list" in {
    P14.duplicateFunctional(List('a, 'b, 'c, 'c, 'd)) should equal(List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
    P14.duplicateFunctional(Nil) should equal(Nil)
  }

  "duplicateFunctionalII" should "return a list with elements duplicated in specified list" in {
    P14.duplicateFunctionalII(List('a, 'b, 'c, 'c, 'd)) should equal(List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
    P14.duplicateFunctionalII(List()) should equal(Nil)
  }

  "duplicateFunctionalIII" should "return a list with elements duplicated in specified list" in {
    P14.duplicateFunctionalIII(List('a', 'b', 'c', 'c', 'd')) should equal(List('a', 'a', 'b', 'b', 'c', 'c', 'c', 'c', 'd', 'd'))
    P14.duplicateFunctionalIII(List.empty[String]) should equal(Nil)
  }

  "duplicateRecursive" should "return a list with elements duplicated in specified list" in {
    P14.duplicateRecursive(List(1, 2, 3, 3, 4)) should equal(List(1, 1, 2, 2, 3, 3, 3, 3, 4, 4))
    P14.duplicateRecursive(Nil) should equal(List.empty[Int])
  }

  "duplicateTailRecursive" should "return a list with elements duplicated in specified list" in {
    P14.duplicateTailRecursive(List("a", "b", "c", "c", "d")) should equal(List("a", "a", "b", "b", "c", "c", "c", "c", "d", "d"))
    P14.duplicateTailRecursive(Nil) should equal(Nil)
  }
}