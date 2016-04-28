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

}

case object Empty extends Stream[Nothing]

// 空でないストリームは先頭と末尾で構成される。
// それらはどちらも非正格である。
// 技術的な限界により、これらは名前渡しのパラメータではなく、
// 明示的な強制を必要とするサンクである。
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  // 空でないストリームを作成するためのスマートコンストラクタ。
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
