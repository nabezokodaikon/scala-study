package fpinscala.laziness

import org.scalatest.FlatSpec

class StreamSpec extends FlatSpec {

  behavior of "Stream テスト"

  it should "headOption" in {
    val s = Stream(1, 2, 3)
    val ret = s.headOption
    assert(ret == Some(1))
  }

  it should "EXERCISE 5.1 toList" in {
    val s = Stream(1, 2, 3)
    val l = s.toList
    assert(l == List(1, 2, 3))
  }

  it should "EXERCISE 5.2 take" in {
    val a = Stream(1, 2, 3).take(2).toList
    assert(a == List(1, 2))
  }

  it should "EXERCISE 5.2 drop" in {
    val a = Stream(1, 2, 3).drop(2).toList
    assert(a == List(3))
  }

  it should "EXERCISE 5.3 takeWhile" in {
    val a = Stream(1, 2, 3, 4).takeWhile(_ < 3).toList
    assert(a == List(1, 2))
  }

  it should "EXERCISE 5.4 forAll" in {
    val a = Stream(1, 2, 3).forAll(
      i => { println(s"hello${i}"); i < 2 })
    assert(a == false)
  }

  it should "EXERCISE 5.5 takeWhileViaFoldRight" in {
    val a = Stream(1, 2, 3, 4).takeWhileViaFoldRight(_ < 3).toList
    assert(a == List(1, 2))
  }

  it should "EXERCISE 5.6 headOptionViaFoldRight" in {
    val a = Stream(1, 2, 3, 4).headOptionViaFoldRight
    assert(a == Some(1))
    val b = Stream().headOptionViaFoldRight
    assert(b == None)
  }

  it should "EXERCISE 5.7 map" in {
    val a = Stream(1, 2, 3, 4)
    val b = a.map(_.toString).toList
    assert(b == List("1", "2", "3", "4"))
  }

  it should "EXERCISE 5.7 filter" in {
    val a = Stream(1, 2, 3, 4)
    val b = a.filter(i => i % 2 == 0).toList
    assert(b == List(2, 4))
  }

  it should "EXERCISE 5.7 append" in {
    val a = Stream(1, 2).append(Stream(3, 4)).toList
    assert(a == List(1, 2, 3, 4))
  }

  it should "EXERCISE 5.7 flatMap" in {
    val a = Stream(1, 2, 3).flatMap(i => Stream(i, i)).toList
    assert(a == List(1, 1, 2, 2, 3, 3))
  }

  it should "トレース" in {
    val s = Stream(
      { () => println(s"Stream${1}"); 1 },
      { () => println(s"Stream${2}"); 2 },
      { () => println(s"Stream${3}"); 3 },
      { () => println(s"Stream${4}"); 4 }
    ).map(i => {
        println(s"map${i}")
        i() + 10
      }
      ).filter(i => {
        println(s"filter${i}")
        i % 2 == 0
      }
      )
    println(s"StreamResult${s}")

    val l = List(
      { () => println(s"List${1}"); 1 },
      { () => println(s"List${2}"); 2 },
      { () => println(s"List${3}"); 3 },
      { () => println(s"List${4}"); 4 }
    ).map(i => {
        println(s"map${i}")
        i() + 10
      }
      ).filter(i => {
        println(s"filter${i}")
        i % 2 == 0
      }
      )
    println(s"ListResult${l}")
  }

  it should "find" in {
    val s = Stream(
      { () => println(s"find${1}"); 1 },
      { () => println(s"find${2}"); 2 },
      { () => println(s"find${3}"); 3 },
      { () => println(s"find${4}"); 4 }
    )
    val a = s.find(_() == 2)
    println(s"${a}")
  }

  it should "EXERCISE 5.9 from" in {
    val a = Stream.from(5)
    assert(a.take(5).toList == List(5, 6, 7, 8, 9))
  }

  it should "EXERCISE 5.10 fibs" in {
    val a = Stream.fibs().take(10).toList
    assert(a == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  it should "EXERCISE 5.10 fibs_1" in {
    val a = Stream.fibs_1.take(10).toList
    assert(a == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  it should "EXERCISE 5.12 fibsViaUnfold" in {
    val a = Stream.fibsViaUnfold.take(10).toList
    assert(a == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  it should "EXERCISE 5.12 fromViaUnfold" in {
    val a = Stream.fromViaUnfold(5).take(5).toList
    assert(a == List(5, 6, 7, 8, 9))
  }

  it should "EXERCISE 5.12 constantViaUnfold" in {
    val a = Stream.constantViaUnfold("A").take(5).toList
    assert(a == List("A", "A", "A", "A", "A"))
  }

  it should "EXERCISE 5.13 mapViaUnfold" in {
    val a = Stream(1, 2, 3).map(_.toString).toList
    assert(a == List("1", "2", "3"))
  }

  it should "EXERCISE 5.13 takeViaUnfold" in {
    val a = Stream.fromViaUnfold(5).takeViaUnfold(5).toList
    assert(a == List(5, 6, 7, 8, 9))
  }

  it should "EXERCISE 5.13 takeWhileViaUnfold" in {
    val a = Stream(1, 3, 2, 5).takeWhileViaUnfold(_ % 2 == 1).toList
    assert(a == List(1, 3))
  }

  it should "EXERCISE 5.13 zipWith" in {
    val a = Stream(1, 2, 3)
    val b = Stream(4, 5, 6)
    val r = a.zipWith(b)(_ + _)
    assert(r.toList == List(5, 7, 9))
  }

  it should "EXERCISE 5.13 zipAll" in {
    val a = Stream(1, 2, 3).zipAll(Stream("a", "b", "c")).toList
    assert(a == List((Some(1), Some("a")), (Some(2), Some("b")), (Some(3), Some("c"))))
    val b = Stream(1, 2).zipAll(Stream("a", "b", "c")).toList
    assert(b == List((Some(1), Some("a")), (Some(2), Some("b")), (None, Some("c"))))
    val c = Stream(1, 2, 3).zipAll(Stream("a", "b")).toList
    assert(c == List((Some(1), Some("a")), (Some(2), Some("b")), (Some(3), None)))
  }

  it should "EXERCISE 5.13 zipAll_1" in {
    val a = Stream(1, 2, 3).zipAll_1(Stream("a", "b", "c")).toList
    assert(a == List((Some(1), Some("a")), (Some(2), Some("b")), (Some(3), Some("c"))))
    val b = Stream(1, 2).zipAll_1(Stream("a", "b", "c")).toList
    assert(b == List((Some(1), Some("a")), (Some(2), Some("b")), (None, Some("c"))))
    val c = Stream(1, 2, 3).zipAll_1(Stream("a", "b")).toList
    assert(c == List((Some(1), Some("a")), (Some(2), Some("b")), (Some(3), None)))
  }

  it should "EXERCISE 5.14 startsWith" in {
    assert(Stream(1, 2, 3).startsWith(Stream(1, 2)) == true)
    assert(Stream(1, 2, 3).startsWith(Stream(2, 3)) == false)
  }
}
