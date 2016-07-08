package com.abirotti.functionalScala

import scala.{Either => E}

object Eithers {

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(a) => Right(f(a))
      case Left(a) => Left(a)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => f(a)
      case Left(a) => Left(a)
    }
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]


}
