package com.abirotti.functionalScala

import org.scalatest.{FunSuite, ShouldMatchers}

class OptionsTest extends FunSuite with ShouldMatchers{

  test("None.map(_+1) should be None") {
    None.map(v => v.toString) should be(None)
  }

  test("Some(1).map(_+1) should be Some(2)"){
    Some(1).map(v => v+1) should be (Some(2))
  }

  test("None.getOrElse(Some(1)) should be Some(1)"){
    None.getOrElse(1) should be(1)
  }

  test("Some(1).getOrElse(Some(2)) should be Some(1)"){
    Some(1).getOrElse(2) should be (1)
  }

  test("None.orElse should return the other option"){
    None.orElse(Some(1)) should be(Some(1))
  }

  test("Some(1).orElse should return the other option"){
    Some(1).orElse(Some(2)) should be(Some(1))
  }

  test("filter on None should be None"){
    None.filter(_ == 2) should be(None)
  }

  test("filter"){
    Some(1).filter(_ == 2) should be(None)
    Some(1).filter(_ == 1) should be(Some(1))
  }

  test("flatMap"){
    None.flatMap(someIfEven) should be(None)
    Some(1).flatMap(someIfEven) should be(None)
    Some(2).flatMap(someIfEven) should be(Some(2))
  }

  def someIfEven: (Int) => Optionz[Int] = v => if (v % 2 == 0) Some(v) else None

  test("variance"){1 should be(2)}
}
