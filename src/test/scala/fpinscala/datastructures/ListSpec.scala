package fpinscala.datastructures

import org.scalatest.FlatSpec

class ListSpec extends FlatSpec {

  behavior of "List テスト"

  it should "EXERCISE 3.16 add1" in {
    val l = List(1, 2, 3)
    val ret = List.add1(l)
    assert(ret === List(2, 3, 4))
  }

  it should "EXERCISE 3.18 map" in {
    val l = List(1, 2, 3)
    val ret = List.map(l)(a => a + 2)
    assert(ret === List(3, 4, 5))
  }

  it should "EXERCISE 3.19 filter" in {
    val l = List(1, 2, 3, 4, 5)
    val ret = List.filter(l)(a => a % 2 == 0)
    assert(ret === List(2, 4))
  }

  it should "EXERCISE 3.20 flatMap" in {
    val l = List(1, 2, 3)
    val ret = List.flatMap(l)(i => List(i, i))
    assert(ret === List(1, 1, 2, 2, 3, 3))
  }

  it should "EXERCISE 3.21 filterViaFlatMap" in {
    val l = List(1, 2, 3, 4, 5)
    val ret = List.filterViaFlatMap(l)(i => i > 3)
    assert(ret === List(4, 5))
  }

  it should "EXERCISE 3.22 addPairwise" in {
    val a = List(1, 2, 3)
    val b = List(4, 5, 6)
    val ret = List.addPairwise(a, b)
    assert(ret === List(5, 7, 9))
  }

  it should "EXERCISE 3.23 zipWith" in {
    val a = List(1, 2, 3)
    val b = List(4, 5, 6)
    val ret = List.zipWith(a, b)((i, j) => i * j)
    assert(ret === List(4, 10, 18))
  }

  it should "EXERCISE 3.24 hasSubsequence" in {
    val l = List(1, 2, 3, 4)
    val a = Nil
    assert(List.hasSubsequence(l, a))
    val b = List(1, 3)
    assert(!List.hasSubsequence(l, b))
    val c = List(2, 3)
    assert(List.hasSubsequence(l, c))
  }
}
