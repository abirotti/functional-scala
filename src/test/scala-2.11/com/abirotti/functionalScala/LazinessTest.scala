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

  test("take(n) takes the first n elements of the stream") {
    tenNumbers.take(3).toList should be((1 to 3).toList)
  }
}
