package fpinscala.laziness

import org.scalatest.FlatSpec

class StreamSpec extends FlatSpec {

  behavior of "Stream テスト"

  it should "headOption" in {
    val s = Stream(1, 2, 3)
    val ret = s.headOption
    assert(ret == Some(1))
  }

  it should "EXERCIZE 5.1 toList" in {
    val s = Stream(1, 2, 3)
    val l = s.toList
    assert(l == List(1, 2, 3))
  }

  it should "EXERCIZE 5.2 take" in {
    val a = Stream(1, 2, 3).take(2).toList
    assert(a == List(1, 2))
  }

  it should "EXERCIZE 5.2 drop" in {
    val a = Stream(1, 2, 3).drop(2).toList
    assert(a == List(3))
  }

  it should "EXERCIZE 5.3 takeWhile" in {
    val a = Stream(1, 2, 3, 4).takeWhile(_ < 3).toList
    assert(a == List(1, 2))
  }

  it should "EXERCIZE 5.4 forAll" in {
    val a = Stream(1, 2, 3).forAll(
      i => { println(s"hello${i}"); i < 2 })
    assert(a == false)
  }

  it should "EXERCIZE 5.5 takeWhileViaFoldRight" in {
    val a = Stream(1, 2, 3, 4).takeWhileViaFoldRight(_ < 3).toList
    assert(a == List(1, 2))
  }
}
