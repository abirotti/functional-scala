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

  def appendListToList[A](a1: List[A], a2: List[A]): List[A] =
  a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, appendListToList(t, a2))
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

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
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

  def reverse[A](as: List[A]): List[A] = {
    val newList: List[A] = Nil // how do I get rid of this here type?
    foldLeft(as, newList)((z, b) => Cons(b, z))
  }

  def reverse2[A](as: List[A]): List[A] = {
    foldLeft2(as, Nil:List[A])((z, b) => Cons(b, z))
  }

  def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((b,a) => f (a, b)) // is there a better way to do this?

  def foldRight2[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(as), z) ((a, b) => f(b, a))

  def append[A](as: List[A], a:A): List[A] =
    foldLeft(as, Cons(a, Nil))((z, b) => Cons(b,z))

  def concatenate[A](as:List[List[A]]): List[A] =
    foldRight(as, Nil:List[A])(appendListToList)

  def incList(l: List[Int]): List[Int] =
    foldRight2(l, Nil:List[Int])((z,b) => Cons(z+1,b))

  def doublesToString(l: List[Double]): List[String] =
    foldRight2(l, Nil: List[String])((z,b) => Cons(z.toString, b))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight2(as, Nil:List[B])((z,b) => Cons(f(z), b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight2(as, Nil:List[A])((z,b) => if (f(z)) Cons(z, b) else b)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight2(as, Nil:List[B])((z,b) => appendListToList(f(z), b))

  def filterByFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) List(x) else Nil)

  def zipAdd[A](ls1: List[A], ls2: List[A]) = ???


}

