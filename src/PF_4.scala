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
  def orElse[B >: A](ob: => Option[B]): Option[B] = ob match {
    case Some(_) => ob
    case _ => this
  }
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if (f(x)) => this
    case _ => None
  }
}
case class Some[+A](get:A) extends Option[A]
case object None extends Option[Nothing]

object PF_4 {

}
