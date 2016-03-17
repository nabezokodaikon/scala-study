package fpinscala.state

import org.scalatest.FlatSpec

class StateSpec extends FlatSpec {

  behavior of "State テスト"

  it should "nextInt" in {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    val (n2, rng3) = rng2.nextInt
    assert(n1 == rng.nextInt._1)
    assert(n2 == rng2.nextInt._1)
  }

  it should "EXERCIZE 6.1 nonNegativeInt" in {
    val rng = SimpleRNG(0)
    val (n1, rng1) = rng.nonNegativeInt(rng)
    val (n2, rng2) = rng.nonNegativeInt(rng1)
    assert(n1 == rng.nonNegativeInt(rng)._1)
    assert(n2 == rng.nonNegativeInt(rng1)._1)
  }

  it should "EXERCIZE 6.2 double" in {
    val rng = SimpleRNG(0)
    val (n1, rng1) = rng.double(rng)
    val (n2, rng2) = rng.double(rng1)
    assert(n1 == rng.double(rng)._1)
    assert(n2 == rng.double(rng1)._1)
  }
}
