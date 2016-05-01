package fpinscala.laziness

import org.scalatest.FlatSpec

class StreamSpec extends FlatSpec {

  behavior of "Stream テスト"

  it should "headOption" in {
    val s = Stream(1, 2, 3)
    val h = s.headOption
    assert(h == Some(1))
  }

  it should "キャッシュされない処理のサンプル" in {
    // println("not cache")が2回出力されることで確認できる。
    def expensive(x: Int): Int = {
      println("not cache")
      x
    }

    val t = () => Stream(1, 2, 3)
    val x = Cons(() => expensive(1), t)
    val h1 = x.headOption
    val h2 = x.headOption
  }

  it should "キャッシュされる処理のサンプル" in {
    // println("cache")が1回出力されることで確認できる。
    def expensive(x: Int): Int = {
      println("cache")
      x
    }

    val x = Stream(expensive(1), 2, 3)
    val h1 = x.headOption
    val h2 = x.headOption
  }

  it should "EXERCIZE 5.1 toList" in {
    val s = Stream(1, 2, 3)
    val l = s.toList
    assert(l == List(1, 2, 3))
  }

  it should "EXERCIZE 5.2 take" in {
    val s = Stream(1, 2, 3, 4, 5)
    assert(s.take(0).toList == List())
    assert(s.take(1).toList == List(1))
    assert(s.take(2).toList == List(1, 2))
    assert(s.take(5).toList == List(1, 2, 3, 4, 5))
    assert(s.take(6).toList == List(1, 2, 3, 4, 5))
  }

  it should "EXERCIZE 5.2 drop" in {
    val s = Stream(1, 2, 3, 4, 5)
    assert(s.drop(0).toList == List(1, 2, 3, 4, 5))
    assert(s.drop(1).toList == List(2, 3, 4, 5))
    assert(s.drop(5).toList == List())
    assert(s.drop(6).toList == List())
  }

}
