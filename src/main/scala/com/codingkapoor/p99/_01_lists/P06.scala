package com.codingkapoor.p99._01_lists

object P06 {

  // Approach is to divide the list equally and then compare the elements from each of the sub lists.
  // List(1, 2, 3, 2, 1) would be divided into sub lists List(1, 2, 3) and List(3, 2, 1).
  def isPalindrome[A](ls: List[A]): Boolean = {

    val sizeOfSublists = (ls.size / 2.0).ceil.toInt

    val sublist1 = ls take sizeOfSublists

    val sublist2 = if (sizeOfSublists % 2 == 0) { ls drop sizeOfSublists } else { ls drop sizeOfSublists - 1 }

    // Filter the indexes for which elements in the sub lists are not equals i.e., for a Palindrome list 
    // returned Vector would be of size 0. 
    (0 to sizeOfSublists - 1) filter { i => sublist1(i) != sublist2(sublist2.size - 1 - i) } isEmpty

  }

  def isPalindromeBuiltin[A](ls: List[A]): Boolean = ls == ls.reverse

}