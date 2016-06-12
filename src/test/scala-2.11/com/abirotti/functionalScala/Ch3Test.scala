package com.abirotti.functionalScala

import com.abirotti.functionalScala.Ch3.{List, Nil}
import com.abirotti.functionalScala.Ch3.List._
import org.scalatest.{FunSuite, ShouldMatchers}

class Ch3Test extends FunSuite with ShouldMatchers{

  test("drop should drop the given number of elements from the head of the list") {
    drop(List(1,2,3), 0) should be (List(1,2,3))
    drop(List(1,2,3), 1) should be (List(2,3))
    drop(List(1,2,3), 2) should be (List(3))
  }

  test("tail should correctly return the tail of the list") {
    tail(List(1,2,3)) should be (List(2,3))
    val exceptionOnEmptyList = intercept[UnsupportedOperationException] {
      tail(List())
    }
    exceptionOnEmptyList.getMessage should be("cannot perform tail on empty list")
  }

  test("dropWhile should drop elements at the head of the list as long as the predicate holds for them") {
    dropWhile(List(1,2,3,4,5,6,7)) (a => a < 4) should be(List(4,5,6,7))
    dropWhile(List(1,2,3,4,5,6,7)) (a => a > 20) should be(List(1,2,3,4,5,6,7))
  }

  test("init should return all but the last element of the list") {
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

  test("sum by foldLeft should correctly sum the elements in a list") {
    sum3(List(1,2,3,4)) should be (10)
    sum3(List(0)) should be (0)
    sum3(Nil: List[Int]) should be (0)
  }

  test("product by foldLeft should correctly calculate the product of the list's elements") {
    product3(List(1,2,3,4)) should be (24)
    product3(List(0)) should be (0)
    product3(Nil: List[Double]) should be (1)
  }

  test("length by foldLeft should return the size of the list") {
    length3(List(1,2,3,4)) should be (4)
    length3(List(0)) should be (1)
    length3(Nil: List[Double]) should be (0)
  }

  test("reverse by foldLeft should correctly reverse the list") {
    reverse(List()) should be (List())
    reverse(List(1)) should be (List(1))
    reverse(List(0,1,2,3,4)) should be (List(4,3,2,1,0))
  }

  test("append_2 should work as append") {
    val list1 = List(1,2,3)
    val list2 = List(4,5,6)
    append_2(list1, list2) should be(append(list1, list2))
  }

  test("concatenate should correctly concatenate all the lists") {
    val l1 = List(1,2,3)
    val l2 = List(11,22,33)
    val l3 = List(111,222,333)

    concatenate(List(l1, l2, l3)) should be(List(1,2,3,11,22,33,111,222,333))
  }

  test("add1 should add 1 to every element of the list") {
    add1(List(1,2,3,4,5,6,7)) should be(List(2,3,4,5,6,7,8))
  }

  test("asString should turn every element of the list to a string") {
    asString(List(1,2,3,4)) should be(List("1", "2", "3", "4"))
  }

  test("map with + 1 should add 1 to every element of the list") {
    map(List(1,2,3,4,5,6,7))(_ + 1) should be(List(2,3,4,5,6,7,8))
  }

  test("map with toString should turn every element of the list to a string") {
    map(List(1,2,3,4))(_.toString) should be(List("1", "2", "3", "4"))
  }

  test("filter should retain the elements of the list for which the predicate holds") {
    filter(List(1,2,3,4,5,6))(_ % 2  == 0) should be(List(2,4,6))
  }

  test("flatMap should apply the given function and then flatten the result") {
    flatMap(List(1,2,3))(i => List(i,i)) should be(List(1,1,2,2,3,3))
  }
}
