import scala.{List=>SList}
import scala.{Nil=>SNil}

sealed trait Option[+A] {

  //TODO: make flatMap, orElse, filter typecheck without matching

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(x) => f(x)
    case None => None
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case _ => ob
  }
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x) => this
    case _ => None
  }
}

case class Some[+A](get:A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap(x => mean(xs map(y => math.pow( y - x, 2 ))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a flatMap(a2 => b map(b2 => f(a2, b2)))

  def sequence[A](a: SList[Option[A]]): Option[SList[A]] = a match {
    case SNil => Some(SNil)
    case (h :: t) => h flatMap (h1 => sequence(t) map (h1 :: _))
  }

  def traverse[A, B](a: SList[A])(f: A => Option[B]): Option[SList[B]] =
    a match {
      case SNil => Some(SNil)
      case (h :: t) => map2 (f(h), traverse(t)(f)) (_::_)
    }

  def sequence2[A](a: SList[Option[A]]): Option[SList[A]] =
    traverse(a)(b=>b)
}

