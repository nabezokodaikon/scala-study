package fpinscala.parallelism

trait Par[A] {

  /**
   * 評価されていないAを受け取り、
   * それを別のスレッドで評価するための計算を返す。
   */
  def unit[A](a: => A): Par[A]

  /**
   * 並列計算から結果の値を取り出す。
   */
  def get[A](a: Par[A]): A

  /**
   * 2つの並列計算の結果を結合する高階関数。
   */
  def map2[A, B, C](a: => Par[A], b: => Par[B])(f: (A, B) => C): Par[C]
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
}
