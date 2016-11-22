package com.abirotti.functionalScala

sealed trait MyOption[+A] {

  def map[B](f: A => B): MyOption[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def orElse1[B >: A](ob: => MyOption[B]): MyOption[B] = this match {
    case None => ob
    case some@Some(v) => some
  }

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this map (Some(_)) getOrElse ob
  def filter(f: A => Boolean): MyOption[A] = this flatMap (x => if (f(x)) Some(x) else None)
  def variance(xs: Seq[Double]): MyOption[Double] = ???
  def lift[A, B](f: A => B): MyOption[A] => MyOption[B] = _ map f
}

object MyOption {
  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  def sequence[A](a: List[MyOption[A]]): MyOption[List[A]] =
    a match {
      case Nil => Some(Nil)
      case aa :: as => aa flatMap (aaa => sequence(as) map (aaa :: _))
    }

  def traverse[A, B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] = a match {
    case Nil => Some(Nil)
    case anA :: as => f(anA) flatMap (aV => traverse(as)(f) map (aV :: _))
    //    case aa :: aas => map2(f(aa), traverse(aas)(f))(_ :: _)
  }

  def sequence2[A](a: List[MyOption[A]]): MyOption[List[A]] = traverse(a)(b => b)

  def Try[A](a: => A): MyOption[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }
}

case class Some[+A](get: A) extends MyOption[A]
case object None extends MyOption[Nothing]
