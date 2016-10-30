package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P06Spec extends FlatSpec with Matchers {

  "isPalindrome" should "return true if the list is Palindrome" in {
    P06.isPalindrome(List(1, 2, 3, 2, 1)) should equal(true)
  }

  "isPalindrome" should "return false if the list is not Palindrome" in {
    P06.isPalindrome(List(1, 2, 3, 4, 5)) should equal(false)
  }

  "isPalindromeBuiltin" should "return true if the list is Palindrome" in {
    P06.isPalindromeBuiltin(List(1, 2, 3, 2, 1)) should equal(true)
  }

  "isPalindromeBuiltin" should "return false if the list is not Palindrome" in {
    P06.isPalindromeBuiltin(List(1, 2, 3, 4, 5)) should equal(false)
  }
}