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
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECD66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}
