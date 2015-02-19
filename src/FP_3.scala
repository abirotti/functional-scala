import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail:List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](ls: List[A], newHead: A): List[A] = ls match {
    case Nil => Nil
    case Cons(x, xs) => Cons(newHead, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match{
    case Nil => Nil
    case Cons(x, xs) if n == 1 => xs
    case Cons(x, xs) if n > 1 => drop(xs, n - 1)
  }

  def dropWhile[A](l: List[A]) (f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs) (f)
    case anotherList  => anotherList
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(x, xs) => Cons(x, init(xs))
    case _ => Nil
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f) )
    }

  def sum2(ns: List[Int]) = foldRight(ns, 0)(_+_)
  def product2(ns: List[Int]) = foldRight(ns, 1)(_*_)

  def length[A](as: List[A]): Int = foldRight(as, 0)((a, z)=> 1 + z)

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec def loop(as: List[A], z:B): B = as match {
      case Cons(x, xs) => loop(xs, f(z, x))
      case Nil => z
    }
    loop(as, z)
  }

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_+_)
  def product3(ns: List[Int]) = foldLeft(ns, 1)(_*_)
  def length3[A](as: List[A]): Int = foldLeft(as, 0)((z, a)=> 1 + z)

}

object FP_3 {

  def main(args: Array[String]): Unit = {

    val l1 = List(1,2,3,4,5,6,7,8)
    val l2 = List(1,2,3,4)


//    println(List.tail(l1))
//    println(List.setHead(l1, 10))
//    println(List.drop(l1, 3))
//    println(List.dropWhile(l1) (x => x <= 3)) // if it is curried, we don't need to specify the type for x: it is gonna be Int
//    println(List.init(l1))
//    println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
//    println(List.length(Nil))
//    println(List.length3(Nil))
//    println(List.length(l1))
//    println(List.length3(l1))
//
//    println(List.sum3(l1))
//    println(List.sum3(l2))
//    println(List.product3(l1))
  }
}
