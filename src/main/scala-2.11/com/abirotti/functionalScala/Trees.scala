package com.abirotti.functionalScala

object Trees {

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
    case Leaf(v) => l(v)
    case Branch(left, right) => b( fold(left)(l)(b), fold(right)(l)(b) )
  }

  def sizeByFold[A](t: Tree[A]): Int = fold(t)(_=>1)((a,b) => a + b)
  def maximumByFold(t: Tree[Int]): Int = fold(t)(v=>v)((a,b) => a max b)
  def depthByFold[A](t: Tree[A]): Int = fold(t)(v=>1)((a, b) => (a max b) + 1)
}