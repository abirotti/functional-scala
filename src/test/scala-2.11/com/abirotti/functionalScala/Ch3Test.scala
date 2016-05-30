package com.abirotti.functionalScala

import com.abirotti.functionalScala.Ch3.{List, Nil}
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

  test("length"){
    List.length(Nil) should be(0)
    List.length(List(1,2,3,4,5)) should be(5)
  }

  test("foldLeft") {
    foldLeft(Nil: List[Int], 0)((x,y) => x + y) should be(0)
    foldLeft(List(1,2,3,4), 0)((x,y) => x + y) should be(10)
  }

  test("sum3") {
    sum3(List(1,2,3,4)) should be (10)
    sum3(List(0)) should be (0)
    sum3(Nil: List[Int]) should be (0)
  }

  test("product3") {
    product3(List(1,2,3,4)) should be (24)
    product3(List(0)) should be (0)
    product3(Nil: List[Double]) should be (1)
  }

  test("length3") {
    length3(List(1,2,3,4)) should be (4)
    length3(List(0)) should be (1)
    length3(Nil: List[Double]) should be (0)
  }

  test("reverse") {
    reverse(List()) should be (List())
    reverse(List(1)) should be (List(1))
    reverse(List(0,1,2,3,4)) should be (List(4,3,2,1,0))
  }

}
