package com.abirotti.functionalScala

import com.abirotti.functionalScala.MyOption.{Try, map2, sequence, traverse}
import org.scalatest.{FunSuite, ShouldMatchers}

class MyOptionTest extends FunSuite with ShouldMatchers {

  test("None.map(_+1) should be None") {
    None.map(v => v.toString) should be(None)
  }

  test("Some(1).map(_+1) should be Some(2)") {
    Some(1).map(v => v + 1) should be(Some(2))
  }

  test("None.getOrElse(Some(1)) should be Some(1)") {
    None.getOrElse(1) should be(1)
  }

  test("Some(1).getOrElse(Some(2)) should be Some(1)") {
    Some(1).getOrElse(2) should be(1)
  }

  test("None.orElse should return the other option") {
    None.orElse(Some(1)) should be(Some(1))
  }

  test("Some(1).orElse should return the other option") {
    Some(1).orElse(Some(2)) should be(Some(1))
  }

  test("filter on None should be None") {
    None.filter(_ == 2) should be(None)
  }

  test("filter") {
    Some(1).filter(_ == 2) should be(None)
    Some(1).filter(_ == 1) should be(Some(1))
  }

  test("flatMap") {
    None.flatMap(someIfEven) should be(None)
    Some(1).flatMap(someIfEven) should be(None)
    Some(2).flatMap(someIfEven) should be(Some(2))
  }

  def someIfEven: (Int) => MyOption[Int] = v => if (v % 2 == 0) Some(v) else None

  ignore("variance") {
    1 should be(2)
  }

  test("map2 should be None if either of the 2 parameters passed is None") {
    map2(Some(1), None)((a, b) => Some(a + b)) should be(None)
  }

  test("map2 should be correctly apply the function f if both parameters contain something") {
    map2(Some(1), Some(3))((a, b) => a + b) should be(Some(4))
  }

  test("sequence should be None if either of the elements of the list passed is None") {
    sequence(List(Some(1), None)) should be(None)
  }

  test("sequence should wrap the list in an Option if all elements of the list contain something") {
    sequence(List(Some(1), Some(4))) should be(Some(List(1, 4)))
  }

  test("traverse should be None if either of the elements of the list passed is None") {
    traverse(List("1", "one"))(i => Try(i.toInt)) should be(None)
  }

  test("traverse should execute the given function if all elements of the list contain something") {
    traverse(List("1", "2"))(i => Try(i.toInt)) should be(Some(List(1, 2)))
  }
}
