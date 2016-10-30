package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P01Spec extends FlatSpec with Matchers {

  "lastElement method" should "return last element from a list" in {
    P01.lastElement(List(1, 2, 3, 4, 5, 6)) should equal(6)
  }

  it should "throw NoSuchElementException when list passed is empty" in {
    intercept[NoSuchElementException] { P01.lastElement(Nil) }
  }

  "lastElementII method" should "return last element from a list" in {
    P01.lastElementII(List('a', 'b', 'c', 'd', 'e')) should equal('e')
  }

  it should "throw NoSuchElementException when list passed is empty" in {
    intercept[NoSuchElementException] { P01.lastElementII(List()) }
  }

  "lastBuiltin" should "return last element from a list" in {
    P01.lastBuiltin(List("a", "b", "c", "d", "e")) should equal("e")
  }

  it should "throw NoSuchElementException when list passed is empty" in {
    intercept[NoSuchElementException] { P01.lastBuiltin(List.empty[Int]) }
  }
  
  "lastRecursive" should "return last element from a list" in {
    P01.lastRecursive(List("a", "b", "c", "d", "e")) should equal("e")
  }
  
  it should "throw NoSuchElementException when list passed is empty" in {
    intercept[NoSuchElementException] { P01.lastRecursive(List.empty[Int]) }
  }

}