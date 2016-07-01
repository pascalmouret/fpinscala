package chapter3

trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case _ => sys.error("empty list")
  }

  def setHead[A](h: A, l: List[A]): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
    case _ => sys.error("empty list")
  }
}
