package com.abirotti.fpinscala.either

class Either {

  import scala.{Either => _, Left => _, List => _, Option => _, Right => _} // hide std library `Option` and `com.abirotti.fpinscala.either.Either`, since we are writing our own in this chapter

  sealed trait Either[+E,+A] {

    def mean(xs: IndexedSeq[Double]): Either[String, Double] =
      if(xs.isEmpty)
        Left("mean of empty list!")
      else
        Right(xs.sum / xs.length)

    def Try[A](a: => A): Either[Exception, A] =
      try Right(a)
      catch { case c: Exception => Left(c)}

    def map[B](f: A => B): Either[E, B] =
      this match {
        case Right(a) => Right(f(a))
        case Left(e) => Left(e)
      }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
      this match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
      }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
      this match {
        case Right(_) => this
        case Left(_) => b
      }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      this flatMap(a2 => b map(b2 => f(a2, b2)))

  }

  case class Right[+A](get: A) extends Either[Nothing,A]
  case class Left[+E](get: E) extends Either[E,Nothing]

  object Either {
    def sequence[E, A](es: List[Either[E, A]]) = traverse(es)(x=>x)

    def traverse[E,A,B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      as match {
        case Nil => Right(Nil)
        case (h :: t) => (f(h) map2 traverse(t)(f))((a,b)=>(a :: b))
      }
  }
}
