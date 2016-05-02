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
}
