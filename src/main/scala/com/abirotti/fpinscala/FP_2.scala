package com.abirotti.fpinscala

import scala.annotation.tailrec

object FP_2 {

  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, acc: Int, acc1: Int ): Int = {
      if (n == 0) acc
      else loop(n-1, acc1, acc + acc1)
    }
    loop(n, 0, 1)
  }

  def isSorted[A](as: Array[A]) (ordered: (A,A) => Boolean): Boolean = as match {
    case Array(a, b, _*) if as.length > 1 =>
      ordered (a,b) && isSorted(as.tail)(ordered)
    case _ => true
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a:A) => f(g(a))
}
