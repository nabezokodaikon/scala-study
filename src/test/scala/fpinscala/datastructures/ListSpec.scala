package fpinscala.datastructures

import org.scalatest.FlatSpec

class ListSpec extends FlatSpec {

  behavior of "List テスト"

  it should "EXERCISE 3.16 add1" in {
    val l = List(1, 2, 3)
    val ret = List.add1(l)
    assert(ret === List(2, 3, 4))
  }
}
