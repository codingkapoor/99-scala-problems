# 99-scala-problems
Solutions and tests for Phil Gold's [S-99: Ninety-Nine Scala Problems](http://aperiodic.net/phil/scala/s-99/).

## Table of Contents

* [Lists](#lists)
* [Arithmetic](#arithmetic)
* [Logic and Codes](#logic-and-codes)
* [Binary Trees](#binary-trees)
* [Multiway Trees](#multiway-trees)
* [Graphs](#graphs)
* [Miscellaneous](#miscellaneous)

## Lists

### [P01](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P01.scala) (*) Find the last element of a list.
  ``` scala
  scala> last(List(1, 1, 2, 3, 5, 8))
  res0: Int = 8
  ```

### [P02](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P02.scala) (*) Find the last but one element of a list.
  ``` scala
  scala> penultimate(List(1, 1, 2, 3, 5, 8))
  res0: Int = 5
  ```

### [P03](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P03.scala) (*) Find the Kth element of a list.
  ``` scala
  scala> nth(2, List(1, 1, 2, 3, 5, 8))
  res0: Int = 2
  ```

### [P04](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P04.scala) (*) Find the number of elements of a list.
  ``` scala
  scala> length(List(1, 1, 2, 3, 5, 8))
  res0: Int = 6
  ```

### [P05](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P05.scala) (*) Reverse a list.
  ``` scala
  scala> reverse(List(1, 1, 2, 3, 5, 8))
  res0: List[Int] = List(8, 5, 3, 2, 1, 1)
  ```

### [P06](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P06.scala) (*) Find out whether a list is a palindrome.
  ``` scala
  scala> isPalindrome(List(1, 2, 3, 2, 1))
  res0: Boolean = true
  ```

### [P07](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P07.scala) (**) Flatten a nested list structure.
  ``` scala
  scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  res0: List[Any] = List(1, 1, 2, 3, 5, 8)
  ```

### [P08](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P08.scala) (**) Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed. 
  ``` scala
  scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
  ```

### [P09](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P09.scala) (**) Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists. 
  ``` scala
  scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  ```

### [P10](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P10.scala) (*) Run-length encoding of a list.
Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E. 
  ``` scala
  scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  ```

### [P11](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P11.scala) (*) Modified run-length encoding.
Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms. 
  ``` scala
  scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  ```

### [P12](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P12.scala) (**) Decode a run-length encoded list.
Given a run-length code list generated as specified in problem P10, construct its uncompressed version. 
  ``` scala
  scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  ```

### [P13](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P13.scala) (**) Run-length encoding of a list (direct solution).
Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly. 
  ``` scala
  scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  ```

### [P14](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P14.scala) (*) Duplicate the elements of a list.
  ``` scala
  scala> duplicate(List('a, 'b, 'c, 'c, 'd))
  res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  ```

### [P15](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P15.scala) (**) Duplicate the elements of a list a given number of times.
  ``` scala
  scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
  res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  ```

### [P16](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P16.scala) (**) Drop every Nth element from a list.
  ``` scala
  scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  ```

### [P17](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P17.scala) (*) Split a list into two parts.
The length of the first part is given. Use a Tuple for your result. 
  ``` scala
  scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  ```

### [P18](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P18.scala) (**) Extract a slice from a list.
Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0. 
  ``` scala
  scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  res0: List[Symbol] = List('d, 'e, 'f, 'g)
  ```

### [P19](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P19.scala) (**) Rotate a list N places to the left.
  ``` scala
  scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

  scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  ```

### [P20](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P20.scala) (*) Remove the Kth element from a list.
Return the list and the removed element in a Tuple. Elements are numbered from 0. 
  ``` scala
  scala> removeAt(1, List('a, 'b, 'c, 'd))
  res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
  ```

### [P21](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P21.scala) (*) Insert an element at a given position into a list.
  ``` scala
  scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
  res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
  ```

### [P22](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P22.scala) (*) Create a list containing all integers within a given range.
  ``` scala
  scala> range(4, 9)
  res0: List[Int] = List(4, 5, 6, 7, 8, 9)
  ```

### [P23](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P23.scala) (**) Extract a given number of randomly selected elements from a list.
  ``` scala
  scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
  res0: List[Symbol] = List('e, 'd, 'a)
  ```

**Hint:** Use the solution to problem P20

### [P24](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P24.scala) (*) Lotto: Draw N different random numbers from the set 1..M.
  ``` scala
  scala> lotto(6, 49)
  res0: List[Int] = List(23, 1, 17, 33, 21, 37)
  ```

### [P25](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P25.scala) (*) Generate a random permutation of the elements of a list.
**Hint:** Use the solution of problem P23. 

  ``` scala
  scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
  res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
  ```

### [P26](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P26.scala) (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient). For pure mathematicians, this result may be great. But we want to really generate all the possibilities. 

  ``` scala
  scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
  res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
  ```

### [P27](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P27.scala) (**) Group the elements of a set into disjoint subsets.
a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.
  ``` scala
  scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
  res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...
  ```

b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
  ``` scala
  scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
  res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...
  ```

Note that we do not want permutations of the group members; i.e. ((Aldo, Beat), ...) is the same solution as ((Beat, Aldo), ...). However, we make a difference between ((Aldo, Beat), (Carla, David), ...) and ((Carla, David), (Aldo, Beat), ...).

You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".

### [P28](https://github.com/codingkapoor/99-scala-problems/blob/master/src/main/scala/com/codingkapoor/p99/_01_lists/P28.scala) (**) Sorting a list of lists according to length of sublists.
a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of the list according to their length. E.g. short lists first, longer lists later, or vice versa.
  ``` scala
  scala> lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
  res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
  ```

b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements according to their length frequency; i.e. in the default, sorting is done ascendingly, lists with rare lengths are placed, others with a more frequent length come later.
  ``` scala
  scala> lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
  res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
  ```

Note that in the above example, the first two lists in the result have length 4 and 1 and both lengths appear just once. The third and fourth lists have length 3 and there are two list of this length. Finally, the last three lists have length 2. This is the most frequent length.

## Arithmetic

## Logic and Codes

## Binary Trees

## Multiway Trees

## Graphs

## Miscellaneous
