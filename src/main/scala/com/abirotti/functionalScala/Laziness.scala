package com.abirotti.functionalScala

import com.abirotti.functionalScala.MyStream.{cons, empty, unfold}

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

  def append[B >: A](s: => MyStream[B]): MyStream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => MyStream[B]): MyStream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  def map2[B](f: A => B): MyStream[B] =
    unfold(this) {
      case Cons(h, t) => So((f(h()), t()))
      case _ => No
    }

  def take2(i: Int): MyStream[A] =
    unfold((this, i)) {
      case ((Cons(h, t), 1)) => So(h(), (t(), 0))
      case ((Cons(h, t), n)) if n > 1 => So(h(), (t(), n - 1))
      case _ => No
    }

  def takeWhile2(p: A => Boolean): MyStream[A] =
    unfold(this) {
      case (Cons(h, t)) if p(h()) => So(h(), t())
      case _ => No
    }

  def zipWith[B, C](b: MyStream[B])(f: (A, B) => C): MyStream[C] =
    unfold(this, b) {
      case (Cons(h1, t1), Cons(h2, t2)) => So(f(h1(), h2()), (t1(), t2()))
      case (_, _) => No
    }

  def zipAll[B](s2: MyStream[B]): MyStream[(Option[A], Option[B])] =
    unfold(this, s2) {
      case (Cons(h1, t1), Cons(h2, t2)) => So( (So(h1()), So(h2())), (t1(), t2()) )
      case (Empty, Cons(h2, t2)) => So( (Op.empty[A], So(h2())), (empty, t2()) )
      case (Cons(h1, t1), Empty) => So( (So(h1()), Op.empty[B]), (t1(), Empty) )
      case _ => No
    }
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

    fibsInternal(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Op[(A, S)]): MyStream[A] =
    f(z) match {
      case So((a, s)) => cons(a, unfold(s)(f))
      case No => empty
    }

  def fibs2: MyStream[Int] = unfold((0, 1)) { case (f, s) => So((f, (s, f + s))) }

  def constant2[A](a: A): MyStream[A] = unfold(a)(n => So(n, n))

  def ones2: MyStream[Int] = unfold(1)(n => So(n, n))
}
