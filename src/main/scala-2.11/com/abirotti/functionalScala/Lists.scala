package com.abirotti.functionalScala

import scala.annotation.tailrec

object Lists {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    def append[A](a1: List[A], a2: List[A]): List[A] =
      a1 match {
        case Nil => a2
        case Cons(h, t) => Cons(h, append(t, a2))
      }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def sum2(ns: List[Int]) =
      foldRight(ns, 0)((x, y) => x + y)

    def product2(ns: List[Double]) =
      foldRight(ns, 1.0)(_ * _)


    def tail[A](l: List[A]): List[A] = l match {
      case Nil => throw new UnsupportedOperationException("cannot perform tail on empty list")
      case Cons(_, xs) => xs
    }

    def tailOption[A](l: List[A]): MyOption[List[A]] = l match {
      case Nil => None
      case Cons(_, xs) => Some(xs)
    }

    def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n <= 0) l
      else l match {
        case Nil => Nil
        case Cons(_, tail) => drop(tail, n - 1)
      }
    }

    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case z@Cons(a, as) => if (f(a)) dropWhile(as)(f) else z
    }

    def init[A](l: List[A]): List[A] = {
      def initInternal(list: List[A], acc: List[A] = Nil): List[A] = list match {
        case Nil => Nil
        case init@Cons(a, Nil) => acc
        case Cons(a, as) => initInternal(as, append(acc, List(a)))
      }
      initInternal(l)
    }

    def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
      @tailrec
      def foldLeftInt(list: List[A], acc: B): B = list match {
        case Nil => acc
        case Cons(x, xs) => foldLeftInt(xs, f(acc, x))
      }
      foldLeftInt(l, z)
    }

    def sum3(ns: List[Int]) = foldLeft(ns, 0)((x, y) => x + y)
    def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
    def length3[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

    def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((list, element) => append(List(element), list))

    def foldRightByFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b, a) => f(a, b))

    def append_2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))
    def concatenate[A](lists: List[List[A]]): List[A] = foldRight(lists, Nil: List[A])(append)
    def add1(list: List[Int]): List[Int] = foldRight(list, Nil: List[Int])((a, b) => Cons(a + 1, b))

    def asString[A](list: List[A]): List[String] =
      foldRight(list, Nil: List[String])((a, b) => Cons(a.toString, b))

    def map[A, B](l: List[A])(f: A => B): List[B] =
      foldRight(l, Nil: List[B])((a, b) => Cons(f(a), b))

    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      foldRight(as, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concatenate(map(as)(f))
    def filter1[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if (f(x)) List(x) else Nil)

    def add(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(a, as), Cons(b, bs)) => Cons(a + b, add(as, bs))
    }

    def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
    }
  }
}
