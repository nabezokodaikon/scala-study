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

  it should "EXERCIZE 6.3 intDouble" in {
    val rng = SimpleRNG(0)
    val ((i1, d1), rng1) = rng.intDouble(rng)
    val ((i2, d2), rng2) = rng.intDouble(rng1)
    assert(i1 == rng.intDouble(rng)._1._1)
    assert(d1 == rng.intDouble(rng)._1._2)
    assert(i1 != i2)
    assert(d1 != d2)
  }

  it should "EXERCIZE 6.3 doubleInt" in {
    val rng = SimpleRNG(0)
    val ((d1, i1), rng1) = rng.doubleInt(rng)
    val ((d2, i2), rng2) = rng.doubleInt(rng1)
    assert(d1 == rng.doubleInt(rng)._1._1)
    assert(i1 == rng.doubleInt(rng)._1._2)
    assert(d1 != d2)
    assert(i1 != i2)
  }

  it should "EXERCIZE 6.3 double3" in {
    val rng = SimpleRNG(0)
    val ((d1, d2, d3), r1) = rng.double3(rng)
    val ((d1a, d2a, d3a), r2) = rng.double3(r1)
    assert(d1 == rng.double3(rng)._1._1)
    assert(d2 == rng.double3(rng)._1._2)
    assert(d3 == rng.double3(rng)._1._3)
    assert(d1 != d1a)
    assert(d2 != d2a)
    assert(d3 != d3a)
  }

  it should "EXERCIZE 6.4 ints" in {
    val rng = SimpleRNG(0)
    val (l1, r1) = rng.ints(3)(rng)
    val (l2, r2) = rng.ints(3)(r1)
    assert(l1 == rng.ints(3)(rng)._1)
    assert(l1 != l2)
  }

  it should "EXERCIZE 6.5 doubleViaMap" in {
    val rng = SimpleRNG(0)
    val rnd = rng.doubleViaMap(rng)
    val res1 = rnd(rng)
    val res2 = rnd(res1._2)
    assert(res1._1 == rnd(rng)._1)
    assert(res1._1 != res2._1)
  }

  it should "EXERCIZE 6.9 mapViaFlatMap" in {
    val rng = SimpleRNG(0)
    val res1 = rng.mapViaFlatMap(rng.int)(_.toString)(rng)
    val res2 = rng.mapViaFlatMap(rng.int)(_.toString)(res1._2)
    assert(res1._1 == rng.mapViaFlatMap(rng.int)(_.toString)(rng)._1)
    assert(res1._1 != res2._1)
  }
}
