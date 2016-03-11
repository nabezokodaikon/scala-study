package fpinscala.laziness

trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /** EXERCISE 5.1 */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  /**
   * EXERCISE 5.2
   *
   * Stream の先頭から n 個の要素を取り出す。
   */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  /**
   * EXERCISE 5.2
   *
   * Stream の先頭からn個の要素をスキップする。
   */
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /**
   * EXERCISE 5.3
   *
   * Stream の先頭から指定された述語とマッチする要素を全て取り出す。
   */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  /**
   * Boolean 関数とマッチする要素がこの Stream に存在するかどうかをチェックします。
   */
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def exists_1(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /**
   * 汎用的な再帰関数
   */
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  /**
   * EXERCISE 5.4
   * 指定された述語とマッチするもの全てをチェックする。
   * マッチしない値が検出された時点でチェックを終了する。
   */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  /**
   * EXERCISE 5.5
   *
   * foldRight を使用して Stream の先頭から指定された述語とマッチする要素を全て取り出す。
   */
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) =>
      if (p(a)) cons(a, b)
      else empty
    )

  /**
   * EXERCISE 5.6
   *
   * foldRight を使用して Stream の先頭の要素を取り出す。
   */
  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  /**
   * EXERCISE 5.7
   */
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  /**
   * EXERCISE 5.7
   */
  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else t
    )

  /**
   * EXERCISE 5.7
   */
  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  /**
   * EXERCISE 5.7
   */
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  /**
   * マッチする要素が存在する場合にその最初の要素だけを返す。
   */
  def find(p: A => Boolean): Option[A] =
    filter(p).headOption
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  /**
   * EXERCISE 5.8
   * 指定された値の無限ストリームを返す。
   */
  def constant[A](a: A): Stream[A] = {
    lazy val t: Stream[A] = Cons(() => a, () => t)
    t
  }

  /**
   * EXERCISE 5.9
   * n で始まって n + 1, n + 2 と続く整数の無限ストリームを生成する。
   */
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  /**
   * EXERCISE 5.10
   * フィボナッチ数列の無限ストリームを生成する。
   */
  def fibs(): Stream[Int] = {
    def fib(n: Int): Int = n match {
      case 0 => 0
      case 1 => 1
      case _ => fib(n - 2) + fib(n - 1)
    }
    from(0).map(fib(_))
  }

  val fibs_1 = {
    def go(f0: Int, f1: Int): Stream[Int] = {
      cons(f0, go(f1, f0 + f1))
    }
    go(0, 1)
  }
}
