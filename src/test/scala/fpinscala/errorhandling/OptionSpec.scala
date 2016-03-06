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
}
