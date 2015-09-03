package com.abirotti.fpinscala.laziness

import org.scalatest.{ShouldMatchers, FunSuite}

class StreamsTests extends FunSuite with ShouldMatchers{

  val nineNumbers = Stream(1,2,3,4,5,6,7,8,9)
  val emptyStream = Stream()

  test("toList should return a list of the stream") {
    nineNumbers.toList should be(List(1,2,3,4,5,6,7,8,9))
  }

  test("toList should return Nil on empty stream") {
    emptyStream.toList should be(Nil)
  }

  test("take(n) should take the first n items of a stream") {
    nineNumbers.take(3).toList should be(List(1,2,3))
  }

  test("take(0) should return the original stream") {
    nineNumbers.take(0) should be(nineNumbers)
  }

  test("drop(n) should return a stream minus the first n items") {
    nineNumbers.drop(3).toList should be(List(4,5,6,7,8,9))
  }

  test("drop(0) should return the same stream") {
    nineNumbers.drop(0) should be(nineNumbers)
  }

  test("takeWhile(p) should take items from the stream as long as p(item) is true") {
    nineNumbers.takeWhile(_ < 6).toList should be(List(1,2,3,4,5))
  }

  test("takeWhile(p) should return empty list if first item doesn't match") {
    nineNumbers.takeWhile(_ % 2 == 0).toList should be(List.empty)
  }

  test("exists should return true if at least an item satisfied the predicate") {
    nineNumbers.exists(_ == 2) should be(true)
  }

  test("exists should return false if no item satisfied the predicate") {
    nineNumbers.exists(_ == 12) should be(false)
  }

  test("forAll should return true if all items satisfied the predicate") {
    nineNumbers.forAll(_ < 10) should be(true)
  }

  test("forAll should return false if not all items satisfied the predicate") {
    nineNumbers.forAll(_ < 8) should be(false)
  }

  test("takeWhileByFoldRight(p) should take items from the stream as long as p(item) is true") {
    nineNumbers.takeWhileByFoldRight(_ < 6).toList should be(List(1,2,3,4,5))
  }

  test("takeWhileByFoldRight(p) should return empty list if first item doesn't match") {
    nineNumbers.takeWhileByFoldRight(_ % 2 == 0).toList should be(List.empty)
  }

  test("headOption of a non-empty stream is something") {
    nineNumbers.headOption should be(Some(1))
  }

  test("headOption of an empty stream is nothing") {
    emptyStream.headOption should be(None)
  }

  test("map!") {
    nineNumbers.map(_ * 2).toList should be(List(2,4,6,8,10,12,14,16,18))
  }

  test("filter!") {
    nineNumbers.filter(_ %2 == 0).toList should be(List(2,4,6,8))
  }

  test("append!") {
    emptyStream.append(Stream(1)).toList should be(List(1))
    nineNumbers.append(Stream(1)).toList should be(List(1,2,3,4,5,6,7,8,9,1))
  }

  test("flatMap!") {
    emptyStream.flatMap(x => Stream(x)).toList should be(Nil)
    nineNumbers.flatMap(x => Stream(x+1)).toList should be(List(2,3,4,5,6,7,8,9,10))
  }
}
