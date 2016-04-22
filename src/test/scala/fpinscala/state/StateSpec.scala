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

  it should "EXERCISE 6.2 double" in {
    import RNG._
    val rng = SimpleRNG(42)
    val (i1, rng2) = double(rng)
    val (i2, _) = double(rng)
    val (i3, _) = double(rng2)
    assert(i1 == i2)
    assert(i1 != i3)
  }

  it should "EXERCISE 6.5 doubleViaMap" in {
    import RNG._
    val rng = SimpleRNG(42)
    val (a1, ra1) = double(rng)
    val (b1, rb1) = doubleViaMap(rng)
    assert(a1 == b1)
    val (a2, _) = double(ra1)
    val (b2, _) = doubleViaMap(rb1)
    assert(a2 == b2)
    assert(b1 != b2)
  }

  it should "EXERCISE 6.6 intDoubleViaMap2" in {
    import RNG._
    val rng = SimpleRNG(42)
    val ((ia1, da1), ra1) = intDouble(rng)
    val ((ib1, db1), rb1) = intDoubleViaMap2(rng)
    assert(ia1 == ib1)
    assert(da1 == db1)
    val ((ia2, da2), _) = intDouble(ra1)
    val ((ib2, db2), _) = intDoubleViaMap2(rb1)
    assert(ia2 == ib2)
    assert(da2 == db2)
    assert(ia1 != ib2)
    assert(da1 != db2)
  }

  it should "EXERCISE 6.7 intsViaSequence" in {
    import RNG._
    val rng = SimpleRNG(42)
    val (a, r) = intsViaSequence(3)(rng)
    assert(a(0) != a(1))
    assert(a(0) != a(2))
    val (b, _) = intsViaSequence(3)(rng)
    assert(a(0) == b(0))
    assert(a(1) == b(1))
    assert(a(2) == b(2))
    val (c, _) = intsViaSequence(3)(r)
    assert(a(0) != c(0))
    assert(a(1) != c(1))
    assert(a(2) != c(2))
  }

  it should "for内包表記" in {
    val rng = RNG.SimpleRNG(42)
    val res1 = State.ns1.run(rng)
    val res2 = State.ns2.run(rng)
    assert(res1._1 == res2._1)
    assert(res1._2 == res2._2)
  }

  it should "EXERCISE 6.11 simulateMachine1" in {
    val machine = Machine(false, 5, 10)
    val inputs = List.fill(4)((Coin, Turn)).flatMap(i => List(i._1, i._2))
    val state = Candy.simulateMachine1(inputs)
    val res = state.run(machine)
    assert(res._1._1 == 14)
    assert(res._1._2 == 1)
  }

  it should "EXERCISE 6.11 simulateMachine2" in {
    val machine = Machine(false, 5, 10)
    val inputs = List.fill(4)((Coin, Turn)).flatMap(i => List(i._1, i._2))
    val state = Candy.simulateMachine2(inputs)
    println("########")
    val res = state.run(machine)
    assert(res._1._1 == 14)
    assert(res._1._2 == 1)
  }
}
