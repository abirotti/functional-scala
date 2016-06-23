package com.abirotti.functionalScala

sealed trait Optionz[+A] {

  def map[B](f: A => B): Optionz[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def flatMap[B](f: A => Optionz[B]): Optionz[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def orElse1[B >: A](ob: => Optionz[B]): Optionz[B] = this match {
    case None => ob
    case some @ Some(v) => some
  }

  def orElse[B >: A](ob: => Optionz[B]): Optionz[B] = this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Optionz[A] = this flatMap(x => if (f(x)) Some(x) else None)

  def variance(xs: Seq[Double]): Optionz[Double] = ???

}
case class Some[+A](get: A) extends Optionz[A]
case object None extends Optionz[Nothing]
