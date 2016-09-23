package com.abirotti.functionalScala

import Stream._
sealed trait Stream[+A] {

  def toList: List[A] = this match {
    case Cons(h, t) =>  h() :: t().toList
    case _ => List()
  }

  def take(i: Int): Stream[A] = this match {
    case Cons(h, t) if i > 1 => cons(h(), t().take(i - 1))
    case Cons(h, t) if i == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(i: Int): Stream[A] = this match {
    case Cons(_, t) if i > 0 => t().drop(i - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) => if(p(h)) cons(h,t) else empty
    )

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => if(p(h())) t().forAll(p) else false
    case Empty => true
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*) )
}
