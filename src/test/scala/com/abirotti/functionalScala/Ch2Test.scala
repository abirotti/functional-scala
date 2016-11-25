package com.abirotti.functionalScala

import com.abirotti.functionalScala.Ch2._
import org.scalatest.{FunSuite, Matchers}

class Ch2Test extends FunSuite with Matchers {

  test("fib should correctly calculate the fibonacci number") {
    fib(1) should be(1)
    fib(2) should be(1)
    fib(3) should be(2)
    fib(4) should be(3)
    fib(10) should be(55)
    fib(100) should be(BigInt("354224848179261915075"))
  }

  test("isSorted should return true for an empty array") {
    isSorted(Array(), alwaysFalse) should be(true)
  }
  test("isSorted should return true for an array of one element") {
    isSorted(Array(1), alwaysFalse) should be(true)
  }
  test("isSorted should return true for a sorted array") {
    isSorted(Array(1, 2, 3, 4, 5, 6), (a1: Int, a2: Int) => a1 <= a2) should be(true)
  }
  test("isSorted should return false for an unsorted array") {
    isSorted(Array(1, 2, 3, 4, 5, 6).reverse, (a1: Int, a2: Int) => a1 <= a2) should be(false)
  }

  private def alwaysTrue(a1: Int = 0, a2: Int = 0) = true
  private def alwaysFalse(a1: Int = 0, a2: Int = 0) = false
  private def sorted(a1: Int, a2: Int): Boolean = a1 <= a2
}
