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

  // EXERCIZE 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if (f(h)) => dropWhile(t, f)
    case _                    => l
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}
