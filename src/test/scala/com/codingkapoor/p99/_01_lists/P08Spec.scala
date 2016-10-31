package com.codingkapoor.p99._01_lists

import org.scalatest.{FlatSpec, Matchers}

class P08Spec extends FlatSpec with Matchers {
  
  "compressFunctional" should "return list that has consecutive duplicates removed from a list" in {
    P08.compressFunctional(List(1, 1, 2, 3, 3, 4, 4, 4, 4)) should equal (List(1, 2, 3, 4))
    P08.compressFunctional(Nil) should equal (Nil)
  }
  
  "compressFunctionalII" should "return list that has consecutive duplicates removed from a list" in {
    P08.compressFunctionalII(List(1, 1, 2, 3, 3, 4)) should equal (List(1, 2, 3, 4))
    P08.compressFunctionalII(Nil) should equal (List())
  }
  
  "compressRecursive" should "return list that has consecutive duplicates removed from a list" in {
    P08.compressRecursive(List(1, 1, 2, 3, 3, 4)) should equal (List(1, 2, 3, 4))
    P08.compressRecursive(List()) should equal (List())
  }
  
  "compressTailRecursive" should "return list that has consecutive duplicates removed from a list" in {
    P08.compressTailRecursive(List(1, 2, 3, 4, 4)) should equal (List(1, 2, 3, 4))
  }
  
  "compressTailRecursiveII" should "return list that has consecutive duplicates removed from a list" in {
    P08.compressTailRecursiveII(List(1, 2, 3, 4, 4)) should equal (List(1, 2, 3, 4))
  }
}