package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P03Spec extends FlatSpec with Matchers {

  "nthElement" should "return 4th element from a list" in {
    P03.nthElement(4, List(1, 2, 3, 4, 5)) should equal(4)
  }

  it should "throw NoSuchElementException when list passed is empty" in {
    intercept[NoSuchElementException](P03.nthElement(4, Nil))
  }

  it should "throw NoSuchElementException when the position specified is greater than the size of the list" in {
    intercept[NoSuchElementException](P03.nthElement(10, List(1)))
  }

  "nthElementII" should "return 2nd element from a list" in {
    P03.nthElementII(2, List(1, 2, 3, 4, 5)) should equal(2)
  }

  it should "throw NoSuchElementException when list passed is empty" in {
    intercept[NoSuchElementException](P03.nthElementII(4, Nil))
  }

  it should "throw NoSuchElementException when the position specified is greater than the size of the list" in {
    intercept[NoSuchElementException](P03.nthElementII(10, List(1)))
  }

  "nthBuiltin" should "return 3rd element from a list" in {
    P03.nthBuiltin(3, List('a', 'b', 'c', 'd')) should equal('c')
  }

  it should "throw NoSuchElementException when list passed is empty" in {
    intercept[NoSuchElementException](P03.nthBuiltin(4, Nil))
  }

  it should "throw NoSuchElementException when the position specified is greater than the size of the list" in {
    intercept[NoSuchElementException](P03.nthBuiltin(10, List(1)))
  }

  "nthBuiltinII" should "return 1st element from a list" in {
    P03.nthBuiltinII(1, List('a', 'b', 'c', 'd')) should equal('a')
  }

  it should "throw NoSuchElementException when list passed is empty" in {
    intercept[NoSuchElementException](P03.nthBuiltinII(4, Nil))
  }

  it should "throw NoSuchElementException when the position specified is greater than the size of the list" in {
    intercept[NoSuchElementException](P03.nthBuiltinII(10, List(1)))
  }

  "nthRecursive" should "return 1st element from a list" in {
    P03.nthRecursive(1, List("a", "b", "c", "d")) should equal("a")
  }
  
  it should "return 1st element from a list that has only one element" in {
    P03.nthRecursive(1, List(1)) should equal(1)
  }
  
  it should "throw NoSuchElementException when list passed is empty" in {
    intercept[NoSuchElementException](P03.nthRecursive(4, Nil))
  }

  it should "throw NoSuchElementException when the position specified is greater than the size of the list" in {
    intercept[NoSuchElementException](P03.nthRecursive(2, List(1)))
  }

}