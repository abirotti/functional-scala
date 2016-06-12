package C2

import C2.Chapter2.{fib, isSorted}
import org.scalatest.{FunSuite, ShouldMatchers}

class Chapter2Test extends FunSuite with ShouldMatchers {

  test("fibonacciLocalRec should calculate the nth Fibonacci number correctly") {
    fib(0) should be(0)
    fib(10) should be(55)
    fib(15) should be(610)
    fib(100) should be(BigInt("354224848179261915075"))
  }

  test("isSorted should return true on empty list") {
    isSorted(List.empty, alwaysFalse) should be (true)
  }

  test("isSorted should return true on a list of 1 element") {
    isSorted(List(1), alwaysFalse) should be (true)
  }

  test("isSorted should recognize an ordered List") {
    isSorted((1 to 20).toList, asc) should be (true)
  }

  test("isSorted should recognize a non ordered List") {
    isSorted((1 to 20).toList.reverse, asc) should be (false)
  }

  def alwaysFalse: (Int, Int) => Boolean = (a: Int, b: Int) => false
  def alwaysTrue: (Int, Int) => Boolean = (a: Int, b: Int) => true
  def asc: (Int, Int) => Boolean = (a: Int, b: Int) => a < b

}
