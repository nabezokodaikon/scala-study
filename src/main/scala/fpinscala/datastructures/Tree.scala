package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // EXERCISE 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // EXERCISE 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  // EXERCISE 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l).max(depth(r)))
  }

  // EXERCISE 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // EXERCISE 3.29
  def fold[A, B](t: Tree[A])(l: A => B)(b: (B, B) => B): B = t match {
    case Leaf(v) => l(v)
    case Branch(x, y) => b(fold(x)(l)(b), fold(y)(l)(b))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(l => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_.max(_))

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 0)((1 + _.max(_)))

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
