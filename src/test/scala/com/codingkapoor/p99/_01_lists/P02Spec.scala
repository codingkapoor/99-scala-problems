package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P02Spec extends FlatSpec with Matchers {

  "secondLastElement" should "return second last element from a list" in {
    P02.secondLastElement(List(1, 2, 3, 4, 5)) should equal(4)
  }

  it should "throw NoSuchElementException when the list passed is empty" in {
    intercept[NoSuchElementException] { P02.secondLastElement(Nil) }
  }

  "secondLastElementII" should "return second last element from a list" in {
    P02.secondLastElementII(List('a', 'b', 'c', 'd', 'e')) should equal('d')
  }

  it should "throw NoSuchElementException when the list passed is empty" in {
    intercept[NoSuchElementException] { P02.secondLastElementII(List()) }
  }

  "secondLastBuiltin" should "return second last element from a list" in {
    P02.secondLastBuiltin(List("a", "b", "c")) should equal("b")
  }

  it should "throw NoSuchElementException when the list passed is empty" in {
    intercept[NoSuchElementException] { P02.secondLastElementII(List.empty[String]) }
  }

  "secondLastBuiltinII" should "return second last element from a list" in {
    P02.secondLastBuiltin(List("a", "b", "c")) should equal("b")
  }

  it should "throw NoSuchElementException when the list passed is empty" in {
    intercept[NoSuchElementException] { P02.secondLastElementII(List.empty[String]) }
  }

  "secondLastRecursive" should "return second last element from a list" in {
    P02.secondLastBuiltin(List("a", "b", "c")) should equal("b")
  }

  it should "throw NoSuchElementException when the list passed is empty" in {
    intercept[NoSuchElementException] { P02.secondLastElementII(List.empty[String]) }
  }

  it should "throw NoSuchElementException when the list passed has only one element" in {
    intercept[NoSuchElementException] { P02.secondLastElementII(List(1)) }
  }

}