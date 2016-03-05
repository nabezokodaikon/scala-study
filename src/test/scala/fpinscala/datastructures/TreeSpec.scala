package fpinscala.datastructures

import org.scalatest.FlatSpec

class TreeSpec extends FlatSpec {

  behavior of "Tree テスト"

  it should "EXERCISE 3.25 add1" in {
    val t = Branch(Branch(Leaf(1), Leaf(1)),
      Branch(Leaf(1), Leaf(1)))
    val ret = Tree.size(t)
    assert(ret == 7)
  }

  it should "EXERCISE 3.26 maximum" in {
    val t = Branch(Branch(Leaf(1), Leaf(100)),
      Branch(Leaf(50), Leaf(8)))
    val ret = Tree.maximum(t)
    assert(ret == 100)
  }

}
