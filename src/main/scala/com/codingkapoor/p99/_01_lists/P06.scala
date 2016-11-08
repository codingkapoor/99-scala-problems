package com.codingkapoor.p99._01_lists

/**
 * 
 * P06 (*) Find out whether a list is a palindrome.
 * Example:
 *
 * scala> isPalindrome(List(1, 2, 3, 2, 1))
 * res0: Boolean = true
 * 
 */
object P06 {

  def isPalindromeBuiltin[T](ls: List[T]): Boolean = ls == ls.reverse

  // Tpproach is to divide the list equally and then compare the elements from each of the sub lists.
  // List(1, 2, 3, 2, 1) would be divided into sub lists List(1, 2, 3) and List(3, 2, 1).
  def isPalindromeFunctional[T](ls: List[T]): Boolean = {

    val sizeOfSublists = (ls.size / 2.0).ceil.toInt

    val sublist1 = ls take sizeOfSublists

    val sublist2 = if (sizeOfSublists % 2 == 0) { ls drop sizeOfSublists } else { ls drop sizeOfSublists - 1 }

    // Filter the indexes for which elements in the sub lists are not equals i.e., for a Palindrome list 
    // returned Vector would be of size 0. 
    (0 to sizeOfSublists - 1) filter { i => sublist1(i) != sublist2(sublist2.size - 1 - i) } isEmpty

    // Or
    // map would return a Vector of booleans which could be reduced to a single boolean result
    // ((0 to sizeOfSublists - 1) map { i => sublist1(i) == sublist2(sublist2.size - 1 - i) } foldLeft true) (_ && _)

    // Or
    // forall returns true if for every i the predicate holds true
    // (0 to sizeOfSublists - 1) forall { i => ls1(i) == ls2(ls2.size - 1 - i) }
  }

  // If we need to reverse the list we can simply do equality comparison instead.
  def isPalindromeFunctionalII[T](ls: List[T]): Boolean = ls zip (ls.reverse) forall { case (a, b) => a == b }

  def isPalindromeRecursive[T](ls: List[T]): Boolean = ls match {

    case Nil                             => true

    case x :: Nil                        => true
    case x :: xs if (x equals (xs.last)) => isPalindromeRecursive(xs dropRight 1)

    case _                               => false
  }

  def isPalindromeRecursiveII[T](ls: List[T]): Boolean = {
    if (ls.isEmpty || ls.size == 1) true
    else if (ls.head == ls.last) isPalindromeRecursiveII(ls.slice(1, ls.size - 1))
    else false
  }

}