package chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// exercises

object Tree {
  def size[A](as: Tree[A]): Int = as match {
    case Leaf(_) => 1
    case Branch(b1, b2) => size(b1) + size(b2)
  }

  def maximum(ints: Tree[Int]): Int = ints match {
    case Leaf(i) => i
    case Branch(b1, b2) => maximum(b1) max maximum(b2)
  }

  def depth[A](as: Tree[A]): Int = as match {
    case Leaf(_) => 0
    case Branch(l, r) => (depth(l) + 1) max (depth(r) + 1)
  }

  def map[A, B](as: Tree[A])(f: A => B): Tree[B] = as match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](as: Tree[A])(fLeaf: (A) => B)(fBranch: (B, B) => B): B = {
    as match {
      case Leaf(a) => fLeaf(a)
      case Branch(l, r) => fBranch(fold(l)(fLeaf)(fBranch), fold(r)(fLeaf)(fBranch))
    }
  }

  def foldSize[A](as: Tree[A]): Int =
    fold(as)(a => 1)(_ + _)

  def foldMaximum(ints: Tree[Int]): Int =
    fold(ints)(identity)(_ max _)

  def foldDepth[A](as: Tree[A]): Int =
    fold(as)(a => 0)((b1, b2) => 1 + (b1 max b2))

  def foldMap[A, B](as: Tree[A])(f: A => B): Tree[B] =
    fold(as)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
