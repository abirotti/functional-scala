package com.abirotti.fpinscala.laziness

import org.scalatest.{ShouldMatchers, FunSuite}

class StreamsTests extends FunSuite with ShouldMatchers{

  val nineNumbers = Stream(1 to 10)
  val emptyStream = Stream()

  test("toList should return a list of the stream") {
    nineNumbers.toList should be(List(1 to 10))
  }

  test("toList should return Nil on empty stream") {
    emptyStream.toList should be(Nil)
  }

  test("drop(n) drops the first n elements of the stream"){
    nineNumbers.drop(3) should be(List(4 to 10))
  }

  test("take(n) takes the first n elements of the stream"){
    nineNumbers.take(3) should be(List(1 to 5))
  }
}
