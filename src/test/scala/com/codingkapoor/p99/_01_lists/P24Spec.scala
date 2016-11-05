package com.codingkapoor.p99._01_lists

import org.scalatest.{ FlatSpec, Matchers }

class P24Spec extends FlatSpec with Matchers {

  "lottoBuiltin" should "return a list with N different random numbers from the set 1..M" in {
    P24.lottoBuiltin(6, 49).size should equal(6)
  }

  "lottoBuiltinII" should "return a list with N different random numbers from the set 1..M" in {
    P24.lottoBuiltinII(6, 49).size should equal(6)
  }

  "lotto" should "return a list with N different random numbers from the set 1..M" in {
    P24.lotto(6, 49).size should equal(6)
  }
}