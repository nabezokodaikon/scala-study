package fpinscala.parallelism

trait Par[A] {

  /**
   * 直ちにa値が得られる計算を作成。
   * 定数値を並列計算に昇格させる。
   */
  def unit[A](a: A): Par[A]

  /**
   * 2つの並列計算の結果を2項関数で結合する。
   */
  def map2[A, B, C](a: => Par[A], b: => Par[B])(f: (A, B) => C): Par[C]

  /**
   * runによる並列評価の対象としてマーク。
   * 計算を並列評価の対象としてマークする。
   * この評価はrunによって強制されるまで実際には発生しない。
   */
  def fork[A](a: => Par[A]): Par[A]

  /**
   * 式aをrunによる並列評価のためのラッピング。
   * 評価されていない引数をParでラッピングし、
   * 並列評価の対象としてマークする。
   */
  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  /**
   * 与えられたParを完全に評価し、forkによって要求される並列計算を生成し、結果の値を取得。
   * 実際に計算を行うことで、Parから値を取得する。
   */
  def run[A](a: Par[A]): A

}

object Par {

  /**
   * リスト 7-1
   */
  def sum_1(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1) {
      ints.headOption.getOrElse(0)
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)
    }

  /**
   * リスト 7-2
   */
  // def sum_2(ints: IndexedSeq[Int]): Int =
  // if (ints.size <= 1) {
  // ints.headOption.getOrElse(0)
  // } else {
  // val (l, r) = ints.splitAt(ints.length / 2)
  // val sumL: Par[Int] = Par.unit(sum(l))
  // val sumR: Par[Int] = Par.unit(sum(r))
  // Par.get(sumL) + Par.get(sumR)
  // }

  /**
   * map2を適用。
   */
  def sum_3(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1) {
      Par.unit(ints.headOption.getOrElse(0))
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }

  /**
   * forkを適用。
   */
  def sum_4(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1) {
      Par.unit(ints.headOption.getOrElse(0))
    } else {
      val (l, r) = ints.splitAt(ints.lenght / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
}
