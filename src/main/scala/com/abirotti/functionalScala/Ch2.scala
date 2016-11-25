package com.abirotti.functionalScala

import scala.annotation.tailrec

object Ch2 {

  def fib(n: Int): BigInt = {
    @tailrec
    def fibInternal(prevPrev: BigInt, prev: BigInt, n: Int): BigInt = n match {
      case 0 => prevPrev
      case 1 => prev
      case _ => fibInternal(prev, prev + prevPrev, n - 1)
    }
    fibInternal(0, 1, n)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def isSortedInternal(a: List[A]): Boolean = a match {
      case _ if a.length < 2 => true
      case anA :: aList => if (ordered(anA, aList.head)) isSortedInternal(aList) else false
    }
    isSortedInternal(as.toList)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => f(a, _)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
