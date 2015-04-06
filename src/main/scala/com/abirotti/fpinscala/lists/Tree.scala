package com.abirotti.fpinscala.lists

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(x) => 0
    case Branch(l, r) => 1 + depth(l) max depth(r)
  }

  def map_t[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map_t(l)(f), map_t(r)(f))
  }

  def fold[A,B](tree: Tree[A])(id: A=>B)(f: (B, B)=>B): B = tree match{
    case Leaf(x) => id(x)
    case Branch(l, r) => f(fold(l)(id)(f), fold(r)(id)(f))
  }

  def sizeViaFold[A](tree: Tree[A]): Int =
    fold(tree)(x=>1)((x, y) => x + y)

  def maximumViaFold(tree: Tree[Int]): Int =
    fold(tree)(x=>x)((x, y) => x max y)

  def depthViaFold[A](tree: Tree[A]): Int =
    fold(tree)(x=>0)((x, y) => 1 + (x max y))

  def map_tViaFold[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(x=>Leaf(f(x)): Tree[B])(Branch(_,_))
}
