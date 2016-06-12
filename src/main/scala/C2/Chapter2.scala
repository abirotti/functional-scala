package C2

import scala.annotation.tailrec

object Chapter2 {

  def fib(number: Int): BigInt= {
    @tailrec
    def fibonacciInt(number: Int, prev: BigInt, next: BigInt): BigInt = {
      if (number == 0) prev
      else if (number == 1) next
      else fibonacciInt(number - 1, next, prev + next)
    }
    fibonacciInt(number, 0, 1)
  }

  def isSorted[A](as: List[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def loop(index: Int): Boolean = {
      if (index >= as.size) true
      else if (!ordered(as(index - 1), as(index))) false
      else loop(index + 1)
    }
    loop(1)
  }

  def curry[A,B,C](f: (A,B) => C): A=>(B=>C) = a=>b=>f(a,b)
  def unCurry[A,B,C](f: A=>B=>C): (A,B)=>C = (a,b)=>f(a)(b)
  def compose[A,B,C](f: B=>C, g: A=>B): A=>C = (a)=>f(g(a))
  def andThen[A,B,C](f: A=>B, g: B=>C) = (a:A)=>g(f(a))
}
