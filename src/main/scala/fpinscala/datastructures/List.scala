package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  // EXERCISE 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil         => sys.error("tail of empty list")
    case Cons(_, xs) => xs
  }

  // EXERCISE 3.3
  def setHead[A](l: List[A], x: A): List[A] = l match {
    case Nil         => sys.error("setHead on empty list")
    case Cons(_, xs) => Cons(x, xs)
  }

  // EXERCISE 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil         => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  // EXERCISE 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if (f(h)) => dropWhile(t, f)
    case _                    => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil        => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  // EXERCISE 3.6
  // リストの末尾を除いたリストを返す。
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil          => sys.error("init on empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t)   => Cons(h, init(t))
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  // EXERCISE 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => 1 + acc)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}
