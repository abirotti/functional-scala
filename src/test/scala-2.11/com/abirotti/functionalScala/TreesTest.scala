package com.abirotti.functionalScala

import com.abirotti.functionalScala.Trees._
import org.scalatest.{FunSuite, ShouldMatchers}

class TreesTest extends FunSuite with ShouldMatchers {

  test("size is 1 on Leaf") {
    Trees.size(Leaf("a")) should be(1)
  }

  test("size of a deeper tree") {
    Trees.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf("a"))) should be(3)
  }

  test("maximum of Leaf(v) is v") {
    maximum(Leaf(1)) should be(1)
  }

  test("maximum of a more complex tree") {
    maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(5)))) should be(5)
  }

  test("depth of Leaf is 1") {
    depth(Leaf(1)) should be(1)
  }

  test("depth of a deeper Tree") {
    val someTree: Branch[Int] = Branch(Leaf(1), Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)))
    depth(someTree) should be(4)
  }

  test("map add1 should add 1 to every node") {
    val someTreeOfOnes: Branch[Int] = Branch(Leaf(1), Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)))
    val someTreeOfTwos: Branch[Int] = Branch(Leaf(2), Branch(Branch(Leaf(2), Leaf(2)), Leaf(2)))
    map(someTreeOfOnes)(_ + 1) should be(someTreeOfTwos)
  }

  test("sizeByFold is 1 on Leaf") {
    sizeByFold(Leaf("a")) should be(1)
  }

  test("sizeByFold of a deeper tree") {
    sizeByFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf("a"))) should be(3)
  }

  test("maximumByFold of Leaf(v) is v") {
    maximumByFold(Leaf(1)) should be(1)
  }

  test("maximumByFold of a more complex tree") {
    maximumByFold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(5)))) should be(5)
  }

  test("depthByFold of Leaf is 1") {
    depthByFold(Leaf(1)) should be(1)
  }

  test("depthByFold of a deeper Tree") {
    val someTree: Branch[Int] = Branch(Leaf(1), Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)))
    depthByFold(someTree) should be(4)
  }
}
