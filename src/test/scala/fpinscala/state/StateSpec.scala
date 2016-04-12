package fpinscala.state

import org.scalatest.FlatSpec

class StateSpec extends FlatSpec {

  behavior of "State テスト"

  it should "randomPair" in {
    import RNG._
    val rng = SimpleRNG(42)
    val ((a1, a2), _) = randomPair(rng)
    val ((b1, b2), rng2) = randomPair(rng)
    val ((c1, c2), _) = randomPair(rng2)
    assert(a1 == b1)
    assert(a2 == b2)
    assert(a1 != c1)
    assert(a2 != c2)
  }

  it should "EXERCISE 6.1 nonNegativeInt" in {
    import RNG._
    val rng = SimpleRNG(42)
    val (i1, rng2) = nonNegativeInt(rng)
    val (i2, _) = nonNegativeInt(rng)
    val (i3, _) = nonNegativeInt(rng2)
    assert(i1 == i2)
    assert(i1 != i3)
  }

}
