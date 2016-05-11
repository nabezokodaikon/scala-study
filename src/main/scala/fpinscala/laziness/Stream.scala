package fpinscala.laziness

object LazinessExample {

  // 引数onTrueとonFalseを関数リテラルで渡している。
  def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
    if (cond) onTrue() else onFalse()

  // if2の引数の渡し方の省略表記。
  def if3[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse

  // 引数iを都度評価するため、iを2回評価している。
  def maybeTwice(b: Boolean, i: => Int) =
    if (b) i + i else 0

  // 引数iをキャッシュするため、iは1回しか評価されない。
  def maybeTwice2(b: Boolean, i: => Int) = {
    lazy val j = i
    if (b) j + j else 0
  }

}

trait Stream[+A] {

  // Streamの先頭を取り出す。
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // EXERCIZE 5.1
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toList2: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def toList3: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  // EXERCIZE 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Empty)
    case _ => Empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // EXERCIZE 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t() takeWhile p)
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def existsViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // 引数型Bの前にある=>矢印は、関数fが第2引数を名前渡しで受け取ることと、
  // それを評価しないという選択が可能であることを意味する。
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      // fが第2引数を評価しない場合、再帰は発生しない。
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  // EXERCIZE 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // EXERCIZE 5.5
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) =>
      if (p(h)) Stream.cons(h, t)
      else Stream.empty[A])

  // EXERCIZE 5.6
  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // EXERCIZE 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) =>
      if (f(h)) Stream.cons(h, t)
      else t)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => Stream.cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => f(h) append t)

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  // EXERCIZE 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (Cons(h, _), 1) => Some((h(), (Empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zip[B](s: Stream[B]): Stream[(A, B)] =
    zipWith(s)((_, _))

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s)((_, _))

  def zipWithAll[B, C](s: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s)) {
      case (Cons(h1, t1), Empty) => Some(f(Some(h1()), Option.empty[B]) -> (t1() -> Stream.empty[B]))
      case (Empty, Cons(h2, t2)) => Some(f(Option.empty[A], Some(h2())) -> (Stream.empty[A] -> t2()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
      case _ => None
    }

  // EXERCIZE 5.14
  def startsWith[B >: A](s: Stream[B]): Boolean =
    zipAll(s).takeWhileViaUnfold(!_._2.isEmpty).forAll {
      case (a, b) => a == b
    }

  // EXERCIZE 5.15
  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(Empty)

  // リストに特定のシーケンスが含まれているかどうかをチェックする。
  def hasSubsequence[B >: A](s: Stream[B]): Boolean =
    tails.exists(_.startsWith(s))
}

case object Empty extends Stream[Nothing]

// 空でないストリームは先頭と末尾で構成される。
// それらはどちらも非正格である。
// 技術的な限界により、これらは名前渡しのパラメータではなく、
// 明示的な強制を必要とするサンクである。
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  // 空でないストリームを作成するためのスマートコンストラクタ。
  // 名前渡しされた引数は、評価が最初に参照されるときまで先送りされるように保持してから
  // Consに渡すようにしている。
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    // 評価の繰り返しを避けるために、headとtailを遅延値としてキャッシュ。
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  // 特定の型の空ストリームを作成するためのスマートコンストラクタ。
  def empty[A]: Stream[A] = Empty

  // 複数の要素からStreamを作成するための、可変長引数を持つ便利なメソッド。
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  // 無限に続くStreamを生成する。
  val ones: Stream[Int] = Stream.cons(1, ones)

  // EXERCIZE 5.8
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  // EXERCIZE 5.9
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  // EXERCIZE 5.10
  val fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

  // EXERCIZE 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case _ => empty
    }

  // EXERCIZE 5.12
  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n) { case n => Some((n, n + 1)) }

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a) { case _ => Some((a, a)) }

  val onesViaUnfold: Stream[Int] =
    unfold(1) { case _ => Some((1, 1)) }

}
