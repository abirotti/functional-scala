package com.abirotti.functionalScala

import com.abirotti.functionalScala.MyStream.cons
import scala.{None => No, Option => Op}

sealed trait MyStream[+A] {

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def take(i: Int): MyStream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if i == 0 => Empty
    case Cons(h, t) if i > 0 => cons(h(), t().take(i - 1))
  }

  def drop(i: Int): MyStream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if i > 0 => t().drop(i - 1)
    case x => x
  }

  def takeWhile(p: A => Boolean): MyStream[A] = this match {
    case Empty => Empty
    case Cons(h, _) if !p(h()) => Empty
    case Cons(h, t) => cons(h(), t().takeWhile(p))
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => p(h()) && t().forAll(p)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def takeWhileByFoldRight(p: A => Boolean): MyStream[A] =
    this.foldRight(MyStream.empty[A])((a, z) => if(p(a)) cons(a, z) else Empty)

  def headOption: Option[A] = this.foldRight(Option.empty[A])((a, _) => Option.apply(a))
}

case object Empty extends MyStream[Nothing]

case class Cons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

object MyStream {
  def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: MyStream[A] = Empty

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
