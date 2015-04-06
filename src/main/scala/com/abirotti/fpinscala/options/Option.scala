package com.abirotti.fpinscala.options

import com.abirotti.fpinscala.lists.{List => _, Nil => _}
import scala.{Either => _, Left => _, List => _, Option => _, Right => _}

sealed trait Option[+A] {

  //TODO: make flatMap, orElse, filter typecheck without matching

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(x) => f(x)
    case None => None
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case _ => ob
  }
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x) => this
    case _ => None
  }
}

case class Some[+A](get:A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap(x => mean(xs map(y => math.pow( y - x, 2 ))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a flatMap(a2 => b map(b2 => f(a2, b2)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case (h :: t) => h flatMap (h1 => sequence(t) map (h1 :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case (h :: t) => map2 (f(h), traverse(t)(f)) (_::_)
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(b=>b)
}

