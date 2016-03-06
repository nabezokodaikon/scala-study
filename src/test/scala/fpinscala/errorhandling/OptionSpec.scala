package fpinscala.errorhandling

import org.scalatest.FlatSpec

class OptionSpec extends FlatSpec {

  behavior of "Opiton テスト"

  // it should "リスト41" in {
  // intercept[Exception] {
  // Option.failingFn(12)
  // }
  // }

  it should "EXERCIZE 4.1 map" in {
    val a = Some(1)
    assert(a.map(i => i.toString) === Some("1"))
    assert(None.map(i => i.toString) === None)
  }

  it should "EXERCIZE 4.1 flatMap" in {
    val a = Some(1)
    assert(a.flatMap(i => Some(i.toString)) === Some("1"))
    assert(None.flatMap(i => Some(i.toString)) === None)
  }

  it should "EXERCIZE 4.1 flatMap_1" in {
    val a = Some(1)
    assert(a.flatMap_1(i => Some(i.toString)) === Some("1"))
    assert(None.flatMap_1(i => Some(i.toString)) === None)
  }

  it should "EXERCIZE 4.1 getOrElse" in {
    assert(Some(1).getOrElse(0) == 1)
    assert(None.getOrElse(0) == 0)
  }

  it should "EXERCIZE 4.1 orElse" in {
    assert(Some(1).orElse(Some(0)) == Some(1))
    assert(None.orElse(Some(0)) == Some(0))
  }

  it should "EXERCIZE 4.1 orElse_1" in {
    assert(Some(1).orElse_1(Some(0)) == Some(1))
    assert(None.orElse_1(Some(0)) == Some(0))
  }

  it should "EXERCIZE 4.1 filter" in {
    assert(Some(1).filter(i => i == 1) == Some(1))
    assert(Some(1).filter(i => i == 2) == None)
    assert(None.filter(i => i == 2) == None)
  }

  it should "EXERCIZE 4.2 variance" in {
    val xs = Seq(1.0, 2, 3, 4)
    val ret = Option.variance(xs)
    assert(ret == Some(1.25))
  }

  it should "EXERCIZE 4.3 map2" in {
    val a = Option.map2(Some(2), Some(3))((x, y) => x + y)
    val b = Option.map2_1(Some(2), Some(3))((x, y) => x + y)
    assert(a == Some(5))
    assert(a == b)
  }

  it should "EXERCIZE 4.4 sequence" in {
    val a = List(Some(1), Some(2))
    assert(Option.sequence(a) == Some(List(1, 2)))
    val b = List(Some(1), None)
    assert(Option.sequence(b) == None)
  }

  it should "EXERCIZE 4.4 sequence_1" in {
    val a = List(Some(1), Some(2))
    assert(Option.sequence_1(a) == Some(List(1, 2)))
    val b = List(Some(1), None)
    assert(Option.sequence_1(b) == None)
  }

  it should "EXERCIZE 4.5 traverse" in {
    val l = List("1", "2", "3")
    val r = Option.traverse(l)(i => Option.Try { i.toInt })
    assert(r == Some(List(1, 2, 3)))
  }

  it should "EXERCIZE 4.5 traverse_1" in {
    val l = List("1", "2", "3")
    val r = Option.traverse_1(l)(i => Option.Try { i.toInt })
    assert(r == Some(List(1, 2, 3)))
  }
}
