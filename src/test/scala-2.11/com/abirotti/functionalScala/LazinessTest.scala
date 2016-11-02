package com.abirotti.fpinscala.laziness

import com.abirotti.functionalScala.MyStream
import org.scalatest.{FunSuite, ShouldMatchers}

class LazinessTest extends FunSuite with ShouldMatchers{

  val tenNumbers = MyStream(1 to 10:_*)
  val emptyStream = MyStream()

  test("toList should return a list of the stream") {
    tenNumbers.toList should be((1 to 10).toList)
  }

  test("toList should return Nil on empty stream") {
    emptyStream.toList should be(Nil)
  }

  test("drop(n) drops the first n elements of the stream") {
    tenNumbers.drop(3).toList should be((4 to 10).toList)
  }

  test("drop(n) on an empty stream returns the stream") {
    emptyStream.drop(3).toList should be(Nil)
  }

  test("take(n) takes the first n elements of the stream") {
    tenNumbers.take(3).toList should be((1 to 3).toList)
  }

  test("take(n) on an empty stream returns the stream") {
    emptyStream.take(3).toList should be(Nil)
  }

  test("takeWhile(isLessThan5) takes the first 4 elements of the stream") {
    tenNumbers.takeWhile(_<5).toList should be((1 to 4).toList)
  }

  test("takeWhile(isLessThan5) on an empty stream returns the stream") {
    emptyStream.takeWhile(_=>true).toList should be(Nil)
  }
}
