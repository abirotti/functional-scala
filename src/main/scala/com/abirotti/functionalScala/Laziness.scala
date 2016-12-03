package com.abirotti.functionalScala

import com.abirotti.functionalScala.MyStream.{cons, empty}

import scala.{None => No, Option => Op, Some => So}

sealed trait MyStream[+A] {

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def take(i: Int): MyStream[A] = this match {
    case Empty => Empty
    case Cons(_, _) if i == 0 => Empty
    case Cons(h, t) if i > 0 => cons(h(), t().take(i - 1))
  }

  def drop(i: Int): MyStream[A] = this match {
    case Empty => Empty
    case Cons(_, t) if i > 0 => t().drop(i - 1)
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
    foldRight(empty[A])((a, z) => if (p(a)) cons(a, z) else Empty)

  def headOption: MyOption[A] =
    foldRight(None: MyOption[A])((a, _) => MyOption.Try(a))

  def map[B](f: A => B): MyStream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): MyStream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B>:A](s: => MyStream[B]): MyStream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => MyStream[B]): MyStream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

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

  def constant[A](a: A): MyStream[A] = cons(a, constant(a))

  def from(n: Int): MyStream[Int] = cons(n, from(n + 1))

  // TODO: can you make this tail recursive?
  def fibs: MyStream[Int] = {
    def fibsInternal(first: Int, second: Int): MyStream[Int] =
      cons(first, fibsInternal(second, first + second))
    fibsInternal(0,1)
  }

  def unfold[A, S](z: S)(f: S => Op[(A, S)]): MyStream[A] =
    f(z) match {
      case So((a,s)) => cons(a, unfold(s)(f))
      case No => empty
    }

  def fibs2: MyStream[Int] = unfold((0,1)) { case (f, s) => So((f, (s, f+s))) }

  def constant2[A](a: A): MyStream[A] = unfold(a)(n => So(n, n))

  def ones2: MyStream[Int] = unfold(1)(n => So(n, n))
}
