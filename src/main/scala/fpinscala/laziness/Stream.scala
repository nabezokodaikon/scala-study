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

  /**
   * EXERCISE 5.13
   */
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  /**
   * EXERCISE 5.13
   */
  def takeViaUnfold(n: Int): Stream[A] =
    unfold((n, this)) {
      case (1, Cons(h, t)) => Some((h(), (0, empty)))
      case (n, Cons(h, t)) if n > 1 => Some((h(), (n - 1, t())))
      case _ => None
    }

  /**
   * EXERCISE 5.13
   */
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  /**
   * EXERCISE 5.13
   * 対応する要素同士を足し合わせて新しい Stream を生成する。
   */
  def zipWith[B >: A](s: Stream[B])(f: (B, B) => B): Stream[B] =
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  /**
   * EXERCISE 5.13
   * 対応する要素同士をタプルにして返す。
   * どちらかの Stream に要素が残っている限り、評価を続ける。
   */
  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (empty, t2())))
      case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), empty)))
      case _ => None
    }

  // def zipAll_1[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
  // zipWithAll(s2)((_, _))

  // def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
  // Stream.unfold((this, s2)) {
  // case (Empty, Empty) => None
  // case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
  // case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
  // case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
  // }

  /**
   * EXERCISE 5.14
   * Stream が別の Stream のプレフィックスであるかどうかを調べる。
   */
  def startsWith[B](s: Stream[B]): Boolean =
    this.zipAll(s).takeWhile(!_._2.isEmpty).forAll {
      case (h1, h2) => h1 == h2
    }

  /**
   * EXERCISE 5.15
   * 先頭を除いたコレクションを順次返す。
   */
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s.drop(1)))
    }.append(Stream(empty))

  /**
   * 特定のシーケンスが含まれているかどうかをチェックする。
   */
  def hasSubsequence[B](sub: Stream[B]): Boolean =
    this.tails.exists(_.startsWith(sub))
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

  /**
   * EXERCISE 5.11
   * 汎用的なストリーム生成関数。
   *
   * @param z 初期状態。
   * @param f ストリームの次の値を生成する関数。
   * @return ストリーム。
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case _ => empty
    }

  /**
   * EXERCISE 5.12
   */
  val fibsViaUnfold =
    unfold((0, 1))(n => Some((n._1, (n._2, n._1 + n._2))))

  val fibsViaUnfold_1 =
    unfold((0, 1)) { case (n0, n1) => Some((n0, (n1, n0 + n1))) }

  /**
   * EXERCISE 5.12
   */
  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n + 1)))

  /**
   * EXERCISE 5.12
   */
  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

}
