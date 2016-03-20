package fpinscala.state

trait RNG {

  def nextInt: (Int, RNG)

  /**
   * EXERCIZE 6.1
   *
   * 0 〜 Int.MaxValue のランダムな整数を生成する。
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    if (i < 0) {
      (-(i + 1), r)
    } else {
      (i, r)
    }
  }

  /**
   * EXERCIZE 6.2
   *
   * 0 〜 1(1 を含まない)の Double 型の値を生成する。
   */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  /**
   * EXERCIZE 6.3
   *
   * ペア(Int, Double)を生成する。
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = rng.double(r1)
    ((i, d), r2)
  }

  /**
   * EXERCIZE 6.3
   *
   * ペア(Double, Int)を生成する。
   */
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = rng.intDouble(rng)
    ((d, i), r)
  }

  /**
   * EXERCIZE 6.3
   *
   * 3要素のタプル(Double, Double, Double)を生成する。
   */
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = rng.double(rng)
    val (d2, r2) = rng.double(r1)
    val (d3, r3) = rng.double(r2)
    ((d1, d2, d3), r3)
  }

  /**
   * EXERCIZE 6.4
   *
   * ランダムな整数リストを生成する。
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count < 1) {
      (Nil, rng)
    } else {
      val (x, r1) = rng.nextInt
      val (xs, r2) = ints(count - 1)(r1)
      (x :: xs, r2)
    }
  }

  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /**
   * EXERCIZE 6.5
   *
   * 0 〜 1(1 を含まない)の Double 型の値を生成する。
   */
  def doubleViaMap(rng: RNG): Rand[Double] = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
  }

  /**
   * EXERCIZE 6.6
   *
   * ra と rb の２つのアクションと、それらの結果を結合する関数 f を受け取り、
   * それらを結合する新しいアクションを返す。
   */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def intDoubleViaBoth: Rand[(Int, Double)] = both(int, double)
  def doubleIntViaBoth: Rand[(Double, Int)] = both(double, int)

  /**
   * EXERCIZE 6.7
   *
   * 遷移の List を1つの遷移にまとめる。
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)((_ :: _)))

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECD66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}
