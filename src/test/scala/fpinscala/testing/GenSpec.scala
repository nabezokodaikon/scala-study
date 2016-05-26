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

  it should "EXERCIZE 8.7 union" in {
    import fpinscala.state._
    val rng = RNG.SimpleRNG(10)
    val g1 = Gen.unit(1)
    val g2 = Gen.unit(2)
    val res1 = Gen.union(g1, g1)
    val res2 = Gen.union(g1, g2)
    assert(res1.sample.run(rng)._1 == g1.sample.run(rng)._1)
    assert(res2.sample.run(rng)._1 == g1.sample.run(rng)._1 ||
      res2.sample.run(rng)._1 == g2.sample.run(rng)._1)
  }

  it should "EXERCIZE 8.8 weighted" in {
    import fpinscala.state._
    val rng = RNG.SimpleRNG(10)
    val d1 = 0.2
    val d2 = 0.4
    val g1 = Gen.unit(1)
    val g2 = Gen.unit(2)
    val res1 = Gen.weighted((g1, d1), (g1, d1))
    val res2 = Gen.weighted((g1, d1), (g2, d1))
    val res3 = Gen.weighted((g1, d1), (g2, d2))
    assert(res1.sample.run(rng)._1 == g1.sample.run(rng)._1)
    assert(res2.sample.run(rng)._1 == g1.sample.run(rng)._1 ||
      res2.sample.run(rng)._1 == g2.sample.run(rng)._1)
    assert(res3.sample.run(rng)._1 == g1.sample.run(rng)._1 ||
      res2.sample.run(rng)._1 == g2.sample.run(rng)._1)
  }

  it should "8.4.1 単純な例" in {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = Prop.forAll(Gen.listOf(smallInt)) { ns =>
      println("########")
      println(ns)
      val max = ns.max
      !ns.exists(_ > max)
    }
  }

}

