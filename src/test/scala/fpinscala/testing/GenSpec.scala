package fpinscala.testing

import org.scalatest.FlatSpec

class GenSpec extends FlatSpec {

  behavior of "Gen テスト"

  it should "EXERCIZE 8.4 choose" in {
    import fpinscala.state._
    val gen = Gen.choose(1, 10)
    val rng1 = RNG.SimpleRNG(10)
    val (n1, rng2) = gen.sample.run(rng1)
    val (n2, _) = gen.sample.run(rng1)
    val (n3, rng3) = gen.sample.run(rng2)
    assert(n1 == n2)
    assert(n1 != n3)
  }

  it should "EXERCIZE 8.4 choose2" in {
    import fpinscala.state._
    val gen = Gen.choose2(1, 10)
    val rng1 = RNG.SimpleRNG(10)
    val (n1, rng2) = gen.sample.run(rng1)
    val (n2, _) = gen.sample.run(rng1)
    val (n3, rng3) = gen.sample.run(rng2)
    assert(n1 == n2)
    assert(n1 != n3)
  }
}

