package chapter3

trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // exercises

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case Nil => sys.error("empty list")
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
    case Nil => sys.error("empty list")
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, t) if n > 0 => drop(t, n - 1)
    case c: Cons[A] => c
    case Nil => sys.error("list too short")
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  // note: this could cause a stack overflow, ideally we'd work with a buffer
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def rightLength[A](as: List[A]): Int =
    foldRight(as, 0)((x,y) => y + 1)

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def sum(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def product(ints: List[Int]): Int =
    foldLeft(ints, 1)(_ * _)

  def length[A](as: List[A]): Int =
    foldLeft(as, 0)((b, a) => b + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil:List[A])((b, a) => Cons(a, b))

  def safeFoldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def append[A](as1: List[A], as2: List[A]) =
    safeFoldRight(as1, as2)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    safeFoldRight(l, Nil:List[A])(append)

  def map[A, B](as: List[A])(f: A => B): List[B] =
    safeFoldRight(as, Nil:List[B])((a, b) => Cons(f(a), b))

  def addOne(ints: List[Int]): List[Int] =
    map(ints)(_ + 1)

  def doubleToString(doubles: List[Double]): List[String] =
    map(doubles)(_.toString)

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    safeFoldRight(as, Nil:List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(as, Nil:List[B])((bs, a) => append(bs, f(a)))

  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if(f(x)) Cons(x, Nil) else Nil)

  def zipInts(ints1: List[Int], ints2: List[Int]): List[Int] = {
    (ints1, ints2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipInts(t1, t2))
    }
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
  }

  // helper for hasSubsequence, not an exercise
  @annotation.tailrec
  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Cons(h, t) =>
      if (startsWith(sup, sub)) true
      else hasSubsequence(t, sub)
    case Nil => sub == Nil
  }
}
