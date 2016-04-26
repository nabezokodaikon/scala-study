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

  it should "EXERCIZE 8.5 unit" in {
    import fpinscala.state._
    val rng = RNG.SimpleRNG(10)
    val gen = Gen.unit("hello")
    val res = gen.sample.run(rng)
    assert(res._1 == "hello")
  }

  it should "EXERCIZE 8.5 boolean" in {
    import fpinscala.state._
    val rng = RNG.SimpleRNG(10)
    val res = Gen.boolean.sample.run(rng)._1
    assert(res == true || res == false)
  }

  it should "EXERCIZE 8.5 listOfN" in {
    import fpinscala.state._
    val rng1 = RNG.SimpleRNG(10)
    val state = State.int
    val gen = Gen(state)
    val (l1, rng2) = Gen.listOfN(3, gen).sample.run(rng1)
    val (l2, _) = Gen.listOfN(3, gen).sample.run(rng1)
    val (l3, _) = Gen.listOfN(3, gen).sample.run(rng2)
    assert(l1 == l2)
    assert(l1 != l3)
  }

  it should "EXERCIZE 8.6 listOfN" in {
    import fpinscala.state._
    val sizeGen = Gen.unit(5)
    val gen = Gen(State.int)
    val listGen = gen.listOfN(sizeGen)
    val (l1, _) = listGen.sample.run(RNG.SimpleRNG(10))
    val (l2, _) = listGen.sample.run(RNG.SimpleRNG(10))
    val (l3, _) = listGen.sample.run(RNG.SimpleRNG(11))
    assert(l1 == l2)
    assert(l1 != l3)
  }
}

