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
}
