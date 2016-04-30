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

}
