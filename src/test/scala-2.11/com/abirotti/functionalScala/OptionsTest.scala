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
}
