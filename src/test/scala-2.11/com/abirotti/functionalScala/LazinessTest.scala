package com.abirotti.functionalScala

import org.scalatest.{FunSuite, ShouldMatchers}

class StreamsTests extends FunSuite with ShouldMatchers {

  val nineNumbers = Stream(1,2,3,4,5,6,7,8,9)
  val emptyStream = Stream()

  test("toList should return a list of the stream") {
    nineNumbers.toList should be(List(1,2,3,4,5,6,7,8,9))
  }

  test("toList should return Nil on empty stream") {
    emptyStream.toList should be(Nil)
  }

  test("drop(n) drops the first n elements of the stream"){
    nineNumbers.drop(3).toList should be(List(4,5,6,7,8,9))
  }

  test("drop(0) returns the stream"){
    nineNumbers.drop(0).toList should be(List(1,2,3,4,5,6,7,8,9))
  }

  test("take(n) takes the first n elements of the stream"){
    nineNumbers.take(3).toList should be(List(1,2,3))
  }

  test("take(0) returns the empty stream"){
    nineNumbers.take(0).toList should be(Nil)
  }

  test("nineNumbers.takeWhile(_<4) returns Stream(1,2,3)") {
    nineNumbers.takeWhile(_<4).toList should be(List(1,2,3))
  }

  test("nineNumbers.takeWhile(_>40) returns empty Stream") {
    nineNumbers.takeWhile(_>40).toList should be(Nil)
  }

  test("nineNumbers.takeWhile2(_<4) returns Stream(1,2,3)") {
    nineNumbers.takeWhile2(_<4).toList should be(List(1,2,3))
  }

  test("nineNumbers.takeWhile2(_>40) returns empty Stream") {
    nineNumbers.takeWhile2(_>40).toList should be(Nil)
  }

  test("nineNumbers.forAll(_%2==0) return false"){
    nineNumbers.forAll(_%2==0) should be(false)
  }

  test("nineNumbers.forAll(_<20) return true"){
    nineNumbers.forAll(_<20) should be(true)
  }
}
