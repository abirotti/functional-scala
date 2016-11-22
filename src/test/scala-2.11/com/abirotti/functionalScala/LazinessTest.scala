package com.abirotti.fpinscala.laziness

import com.abirotti.functionalScala.MyStream
import org.scalatest.{FunSuite, ShouldMatchers}

class LazinessTest extends FunSuite with ShouldMatchers{

  val tenNumbers = MyStream[Int](1 to 10:_*)
  val emptyStream = MyStream[Int]()

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
    emptyStream.takeWhile(_<5).toList should be(Nil)
  }

  test("forAll(isLessThan20) on 1 to 10 returns true") {
    tenNumbers.forAll(_<20) should be(true)
  }

  test("forAll(isLessThan5) on 1 to 10 returns false") {
    tenNumbers.forAll(_<5) should be(false)
  }

  test("forAll(isLessThan5) on an empty stream returns true") {
    emptyStream.forAll(_<20) should be(true)
  }

  test("takeWhileByFoldRight(isLessThan5) takes the first 4 elements of the stream") {
    tenNumbers.takeWhileByFoldRight(_<5).toList should be((1 to 4).toList)
  }

  test("takeWhileByFoldRight(isLessThan5) on an empty stream returns the stream") {
    emptyStream.takeWhileByFoldRight(_<5).toList should be(Nil)
  }

  test("headOption of emptyStream is None") {
    emptyStream.headOption should be(None)
  }

  test("headOption of tenNumbers is Some(1)") {
    tenNumbers.headOption should be(Some(1))
  }
}
