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

  it should "EXERCIZE 5.6 headOptionViaFoldRight" in {
    val a = Stream(1, 2, 3, 4).headOptionViaFoldRight
    assert(a == Some(1))
    val b = Stream().headOptionViaFoldRight
    assert(b == None)
  }

  it should "EXERCIZE 5.7 map" in {
    val a = Stream(1, 2, 3, 4)
    val b = a.map(_.toString).toList
    assert(b == List("1", "2", "3", "4"))
  }

  it should "EXERCIZE 5.7 filter" in {
    val a = Stream(1, 2, 3, 4)
    val b = a.filter(i => i % 2 == 0).toList
    assert(b == List(2, 4))
  }

  it should "EXERCIZE 5.7 append" in {
    val a = Stream(1, 2).append(Stream(3, 4)).toList
    assert(a == List(1, 2, 3, 4))
  }

  it should "EXERCISE 5.7 flatMap" in {
    val a = Stream(1, 2, 3).flatMap(i => Stream(i, i)).toList
    assert(a == List(1, 1, 2, 2, 3, 3))
  }

}
