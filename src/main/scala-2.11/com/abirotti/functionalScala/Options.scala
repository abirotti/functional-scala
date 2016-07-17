package com.abirotti.functionalScala

import scala.{Option => O}

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def orElse1[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case some@Some(v) => some
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob
  def filter(f: A => Boolean): Option[A] = this flatMap (x => if (f(x)) Some(x) else None)
  def variance(xs: Seq[Double]): Option[Double] = ???
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
}

object Option {
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case aa :: as => aa flatMap (aaa => sequence(as) map (aaa :: _))
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case anA :: as => f(anA) flatMap (aV => traverse(as)(f) map (aV :: _))
    //    case aa :: aas => map2(f(aa), traverse(aas)(f))(_ :: _)
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(b => b)

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
