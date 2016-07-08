package com.abirotti.functionalScala

import org.scalatest.{FunSuite, ShouldMatchers}
import com.abirotti.functionalScala.Eithers._

class EitherTests extends FunSuite with ShouldMatchers{

  test("map doesn't apply on the Left") {
    Left(1) map (_ toString) should be(Left(1))
  }

  test("but it does apply on the Right") {
    Right(2) map (_ toString) should be(Right("2"))
  }

  test("orElse on Right returns self") {
    Right(1) orElse (Left(1)) should be(Right(1))
  }

  test("orElse on Left returns other") {
    Left(1) orElse (Left(2)) should be(Left(2))
  }

  test("flatMap on Right applies the function") {
    Right(1) flatMap (a => Left(a)) should be(Left(1))
  }

  test("flatMap on Left returns Left") {
    Left(1) flatMap (a => Right(a)) should be(Left(1))
  }
}
