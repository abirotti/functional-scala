import scala.annotation.tailrec

object FP_2 {

  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, acc: Int, acc1: Int ): Int = {
      if (n == 0) acc
      else loop(n-1, acc1, acc + acc1)
    }
    loop(n, 0, 1)
  }

  def isSorted[A](as: Array[A]) (ordered: (A,A) => Boolean): Boolean = as match {
    case Array(a, b, _*) if as.length > 1 =>
      ordered (a,b) && isSorted(as.tail)(ordered)
    case _ => true
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a:A) => f(g(a))

  def main (args: Array[String]) {
    assert ( isSorted[Int](Array(1,2,1,3,4)) ((a, b)=> a<=b ) == false )
    assert ( isSorted[Int](Array(1,2,3,3,4)) ((a, b)=> a<=b ) == true )
    assert ( isSorted[Int](Array(1,2,3,3,4)) ((a, b)=> a<b ) == false )

    val curried = curry((a:Int, b:Int) => a+b)

    assert( curried(1)(2) == uncurry(curried) (1,2) )

    def double(a:Int) = a * 2
    def increment(b:Int) = b + 1

    assert (compose(increment, double) (2) == 5)
    assert (compose(double, increment) (2) == 6)
  }
}
