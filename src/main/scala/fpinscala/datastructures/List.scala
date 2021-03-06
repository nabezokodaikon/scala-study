package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // EXERCISE 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, xs) => xs
  }

  // EXERCISE 3.3
  def setHead[A](l: List[A], x: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, xs) => Cons(x, xs)
  }

  // EXERCISE 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  // EXERCISE 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if (f(h)) => dropWhile(t, f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  // EXERCISE 3.6
  // リストの末尾を除いたリストを返す。
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("init on empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  // EXERCISE 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => 1 + acc)

  // EXERCISE 3.11
  def sumLeft(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def productLeft(l: List[Double]): Double = {
    foldLeft(l, 1.0)(_ * _)
  }

  def lengthLeft[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => acc + 1)

  // EXERCISE 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // EXERCISE 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  // EXERCISE 3.13
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    val func = foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))
    func(z)
  }

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  // EXERCISE 3.14
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)((a, b) => Cons(a, b))

  // EXERCISE 3.15
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  // EXERCISE 3.16
  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  // EXERCISE 3.17
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  // EXERCISE 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))
  }

  // EXERCISE 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((h, t) => f(h) match {
      case true => Cons(h, t)
      case _ => t
    })
  }

  // EXERCISE 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  // EXERCISE 3.21
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(i => f(i) match {
      case true => List(i)
      case false => Nil
    })

  // EXERCISE 3.22
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  // EXERCISE 3.23
  def zipWith[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  // EXERCISE 2.24
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
    case _ => false
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}
