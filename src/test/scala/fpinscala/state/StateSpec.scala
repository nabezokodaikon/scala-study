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

}
