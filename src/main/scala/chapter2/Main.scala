package chapter2

object Main extends App {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(oldFib: Int, newFib: Int, n: Int): Int = {
      if (n == 0) newFib
      else go(newFib, oldFib + newFib, n - 1)
    }

    go(0, 1, n - 2) // since we already start with the first two numbers
  }

  println(fib(6))
}
