package com.abirotti.functionalScala

import com.abirotti.functionalScala.MyStream.cons

sealed trait MyStream[+A] {

  def toList: List[A] = this match {
    case Cons(h, t) =>  h() :: t().toList
    case _ => List()
  }

  def take(i: Int): MyStream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if i == 0 => Empty
    case Cons(h, t) if i > 0 => cons(h(), t().take(i-1))
  }

  def drop(i: Int): MyStream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if i > 0 => t().drop(i-1)
    case x => x
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
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*) )
}
