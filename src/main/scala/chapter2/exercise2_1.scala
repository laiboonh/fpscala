package chapter2

import scala.annotation.tailrec

object ex2_1 {
  def fib(n: Int): Int = {
    @tailrec
    def go(prev: Int, pprev: Int, n: Int): Int = {
      if (n == 1) 0
      else if (n == 2) 1
      else if (n == 3) prev + pprev
      else go(pprev, pprev + prev, n - 1)
    }

    go(0, 1, n)
  }
}

object ex2_2 {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def go(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) false
      else go(n + 1)
    }

    go(0)
  }
}

object ex2_3 {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }
}

object ex2_4 {
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }
}

object ex2_5 {
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    val fn = f compose g
    a => fn(a)
  }
}