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
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECD66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}
