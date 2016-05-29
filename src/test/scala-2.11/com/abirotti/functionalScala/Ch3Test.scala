package com.abirotti.functionalScala

import com.abirotti.functionalScala.Ch3.List
import com.abirotti.functionalScala.Ch3.List._
import org.scalatest.{FunSuite, ShouldMatchers}

class Ch3Test extends FunSuite with ShouldMatchers{

  test("testDrop") {
    drop(List(1,2,3), 0) should be (List(1,2,3))
    drop(List(1,2,3), 1) should be (List(2,3))
    drop(List(1,2,3), 2) should be (List(3))
  }

  test("testTail") {
    tail(List(1,2,3)) should be (List(2,3))
    val exceptionOnEmptyList = intercept[UnsupportedOperationException] {
      tail(List())
    }
    exceptionOnEmptyList.getMessage should be("cannot perform tail on empty list")
  }

  test("dropWhile") {
    dropWhile(List(1,2,3,4,5,6,7)) (a => a < 4) should be(List(4,5,6,7))
    dropWhile(List(1,2,3,4,5,6,7)) (a => a > 20) should be(List(1,2,3,4,5,6,7))
  }

  test("init") {
    init(List()) should be (List())
    init(List(1,2,3,4,5,6,7)) should be (List(1,2,3,4,5,6))
  }
}
