package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P06Spec extends FlatSpec with Matchers {

  "isPalindrome" should "return true if the list is Palindrome" in {
    P06.isPalindromeFunctional(List(1, 2, 3, 2, 1)) should equal(true)
  }

  "isPalindrome" should "return false if the list is not Palindrome" in {
    P06.isPalindromeFunctional(List(1, 2, 3, 4, 5)) should equal(false)
  }

  "isPalindromeBuiltin" should "return true if the list is Palindrome" in {
    P06.isPalindromeBuiltin(List(1, 2, 3, 2, 1)) should equal(true)
  }

  "isPalindromeBuiltin" should "return false if the list is not Palindrome" in {
    P06.isPalindromeBuiltin(List(1, 2, 3, 4, 5)) should equal(false)
  }

  "isPalindromeBuiltinII" should "return true if the list is Palindrome" in {
    P06.isPalindromeFunctionalII(List(1, 2, 3, 2, 1)) should equal(true)
  }

  "isPalindromeBuiltinII" should "return false if the list is not Palindrome" in {
    P06.isPalindromeFunctionalII(List(1, 2, 3, 4, 5)) should equal(false)
  }

  "isPalindromeRecursive" should "return true if the list is Palindrome" in {
    P06.isPalindromeRecursive(List(1, 2, 3, 2, 1)) should equal(true)
  }

  "isPalindromeRecursive" should "return false if the list is not Palindrome" in {
    P06.isPalindromeRecursive(List(1, 2, 3, 4, 5)) should equal(false)
  }

  "isPalindromeRecursiveII" should "return true if the list is Palindrome" in {
    P06.isPalindromeRecursiveII(List(1, 2, 3, 2, 1)) should equal(true)
  }

  "isPalindromeRecursiveII" should "return false if the list is not Palindrome" in {
    P06.isPalindromeRecursiveII(List(1, 2, 3, 4, 5)) should equal(false)
  }
}