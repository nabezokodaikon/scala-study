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

  it should "EXERCIZE 5.3 takeWhile" in {
    val s = Stream(1, 2, 3, 4, 5)
    assert(s.takeWhile(_ < 3).toList == List(1, 2))
    assert(s.takeWhile(_ % 2 == 1).toList == List(1))
  }

  it should "EXERCIZE 5.4 forAll" in {
    assert(Stream().forAll(a => a == 1) == true)
    assert(Stream(1, 1, 1).forAll(a => a == 1) == true)
    assert(Stream(1, 1, 2).forAll(a => a == 1) == false)
  }

  it should "EXERCIZE 5.5 takeWhileViaFoldRight" in {
    val s = Stream(1, 2, 3, 4, 5)
    assert(s.takeWhileViaFoldRight(_ < 3).toList == List(1, 2))
    assert(s.takeWhileViaFoldRight(_ % 2 == 1).toList == List(1))
  }

  it should "EXERCIZE 5.6 headOptionViaFoldRight" in {
    assert(Empty.headOptionViaFoldRight == None)
    assert(Stream(1, 2, 3).headOptionViaFoldRight == Some(1))
  }

  it should "EXERCIZE 5.7 map" in {
    assert(Stream.empty[Int].map(_.toString).toList == Stream.empty[String].toList)
    assert(Stream(1, 2, 3).map(_.toString).toList == Stream("1", "2", "3").toList)
  }

  it should "EXERCIZE 5.7 filter" in {
    val s = Stream(1, 2, 3, 4, 5)
    assert(s.filter(a => a % 2 == 0).toList == List(2, 4))
    assert(s.filter(a => a % 2 == 1).toList == List(1, 3, 5))
  }

  it should "EXERCIZE 5.7 append" in {
    assert(Stream(1, 2).append(Stream(3, 4, 5)).toList == List(1, 2, 3, 4, 5))
  }

  it should "EXERCIZE 5.7 flatMap" in {
    val s = Stream(Stream(1, 2), Stream(3), Stream(4, 5))
    val l = s.flatMap(i => i).toList
    assert(l == List(1, 2, 3, 4, 5))
  }

  it should "EXERCIZE 5.8 constant" in {
    assert(Stream.constant("a").take(3).toList == List("a", "a", "a"))
  }

  it should "EXERCIZE 5.9 from" in {
    assert(Stream.from(5).take(3).toList == List(5, 6, 7))
  }

  it should "EXERCIZE 5.10 fibs" in {
    val fibs = Stream.fibs
    assert(fibs.take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
  }

  it should "EXERCIZE 5.12 fibsViaUnfold" in {
    val fibs = Stream.fibsViaUnfold
    assert(fibs.take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
  }

  it should "EXERCIZE 5.12 fromViaUnfold" in {
    assert(Stream.fromViaUnfold(0).take(2).toList == List(0, 1))
    assert(Stream.fromViaUnfold(5).take(3).toList == List(5, 6, 7))
  }

  it should "EXERCIZE 5.12 constantViaUnfold" in {
    assert(Stream.constantViaUnfold("a").take(3).toList == List("a", "a", "a"))
  }

  it should "EXERCIZE 5.12 onesViaUnfold" in {
    assert(Stream.onesViaUnfold.take(3).toList == List(1, 1, 1))
  }

  it should "EXERCIZE 5.13 mapViaUnfold" in {
    assert(Stream.empty[Int].mapViaUnfold(_.toString).toList == Stream.empty[String].toList)
    assert(Stream(1, 2, 3).mapViaUnfold(_.toString).toList == Stream("1", "2", "3").toList)
  }

  it should "EXERCIZE 5.13 takeViaUnfold" in {
    val s = Stream(1, 2, 3, 4, 5)
    assert(s.takeViaUnfold(0).toList == List())
    assert(s.takeViaUnfold(1).toList == List(1))
    assert(s.takeViaUnfold(2).toList == List(1, 2))
    assert(s.takeViaUnfold(5).toList == List(1, 2, 3, 4, 5))
    assert(s.takeViaUnfold(6).toList == List(1, 2, 3, 4, 5))
  }

  it should "EXERCIZE 5.13 takeWhileViaUnfold" in {
    val s = Stream(1, 2, 3, 4, 5)
    assert(s.takeWhileViaUnfold(_ < 3).toList == List(1, 2))
    assert(s.takeWhileViaUnfold(_ % 2 == 1).toList == List(1))
  }

  it should "EXERCIZE 5.13 zipWith" in {
    val a = Stream(1, 2, 3)
    val b = Stream(1, 2, 3)
    val r = a.zipWith(b)(_ + _).toList
    assert(r == List(2, 4, 6))
  }

  it should "EXERCIZE 5.13 zipAll" in {
    assert(Stream(1, 2).zipAll(Stream("a")).toList == List((Some(1), Some("a")), (Some(2), None)))
    assert(Stream(1).zipAll(Stream("a", "b")).toList == List((Some(1), Some("a")), (None, Some("b"))))
    assert(Stream(1, 2).zipAll(Stream("a", "b")).toList == List((Some(1), Some("a")), (Some(2), Some("b"))))
  }
}
