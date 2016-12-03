package com.abirotti.functionalScala

import com.abirotti.functionalScala.MyStream._
import org.scalatest.{FunSuite, Matchers}

class LazinessTest extends FunSuite with Matchers{

  val tenNumbers: MyStream[Int] = MyStream[Int](1 to 10:_*)
  val emptyStream: MyStream[Int] = MyStream[Int]()

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

  test("map of empty Stream is empty"){
    emptyStream.map(x=>x) should be(emptyStream)
  }

  test("map(_+1) of tenNumbers increases each by 1"){
    tenNumbers.map(x=>x+1).toList should be(List[Int](2 to 11:_*))
  }

  test("filter of empty Stream is empty"){
    emptyStream.filter(_%2==0) should be(emptyStream)
  }

  test("filter(isEven) of tenNumbers returns the even numbers"){
    tenNumbers.filter(_%2==0).toList should be(List[Int](2,4,6,8,10))
  }

  test("append tenNumbers to empty stream returns tenNumbers"){
    emptyStream.append(tenNumbers).toList should be(tenNumbers.toList)
  }

  test("append empty to tenNumbers stream returns tenNumbers"){
    tenNumbers.append(emptyStream).toList should be(tenNumbers.toList)
  }

  test("append empty to empty stream returns empty"){
    emptyStream.append(emptyStream).toList should be(Nil)
  }

  test("flatMap should apply the given function and then flatten the result"){
    tenNumbers.flatMap(a => MyStream(a)).toList should be(tenNumbers.toList)
  }

  test("constant returns an infinite stream with given value") {
    constant(3).take(3).toList should be(List(3,3,3))
  }

  test("constant2 returns an infinite stream with given value") {
    constant2(3).take(3).toList should be(List(3,3,3))
  }

  test("from returns an infinite stream of increasing integers starting from the given value") {
    from(1).take(4).toList should be(List(1,2,3,4))
  }

  test("fib should create the fibonacci sequence") {
    fibs.take(7).toList should be(List(0,1,1,2,3,5,8))
  }

  test("fibs2 should create the fibonacci sequence") {
    fibs2.take(7).toList should be(List(0,1,1,2,3,5,8))
  }

  test("ones2 should create a sequence of ones"){
    ones2.take(7).toList should be(List(1,1,1,1,1,1,1))
  }
}
