class Either {

  import scala.{Option => _, Either => _, Left => _, Right => _, List => _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

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
        case Left(e) => Left(e)
        case Right(a) => Right(f(a))
      }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
      this match {
        case Left(e) => Left(e)
        case Right(a) => f(a)
      }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
      this match {
        case Right(_) => this
        case Left(_) => b
      }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      this flatMap(a2 => b map(b2 => f(a2, b2)))

  }

  case class Left[+E](get: E) extends Either[E,Nothing]
  case class Right[+A](get: A) extends Either[Nothing,A]

  object Either {
    def sequence[E, A](es: List[Either[E, A]]) = traverse(es)(x=>x)

    def traverse[E,A,B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      as match {
        case Nil => Right(Nil)
        case Cons(h, t) => (f(h) map2 traverse(t)(f))((a,b)=>Cons(a,b))
      }
  }
}
