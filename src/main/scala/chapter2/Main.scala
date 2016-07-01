package chapter2

object Main extends App {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, oldFib: Int, newFib: Int): Int = {
      if (n == 0) newFib
      else go(n - 1, newFib, oldFib + newFib)
    }

    go(n - 2, 0, 1) // since we already start with the first two numbers
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(i: Int): Boolean = {
      if (i == 1) ordered(as(i - 1), as(1))
      else if (ordered(as(i - 1), as(i))) go(i - 1)
      else false
    }

    go(as.length - 1)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  println(isSorted[Int](Array(1, 2, 3), _ < _))
}
