package fpinscala.datastructures

import org.scalatest.FlatSpec

class TreeSpec extends FlatSpec {

  behavior of "Tree テスト"

  it should "EXERCISE 3.25 size" in {
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

  it should "EXERCISE 3.27 depth" in {
    val t =
      Branch(
        Branch(
          Leaf(1),
          Leaf(100)
        ),
        Branch(Leaf(50),
          Branch(
            Leaf(1),
            Leaf(100)
          )
        )
      )
    val ret = Tree.depth(t)
    assert(ret == 3)
  }

  it should "EXERCISE 3.28 map" in {
    val a =
      Branch(
        Branch(
          Leaf(1),
          Leaf(100)
        ),
        Branch(
          Leaf(50),
          Leaf(8)
        )
      )
    val r = Tree.map(a)(_.toString)
    val b =
      Branch(
        Branch(
          Leaf("1"),
          Leaf("100")
        ),
        Branch(
          Leaf("50"),
          Leaf("8")
        )
      )
    assert(r == b)
  }

  it should "EXERCISE 3.29 sizeViaFold" in {
    val t = Branch(Branch(Leaf(1), Leaf(1)),
      Branch(Leaf(1), Leaf(1)))
    val ret = Tree.sizeViaFold(t)
    assert(ret == 7)
  }

  it should "EXERCISE 3.29 maximumViaFold" in {
    val t = Branch(Branch(Leaf(1), Leaf(100)),
      Branch(Leaf(50), Leaf(8)))
    val ret = Tree.maximumViaFold(t)
    assert(ret == 100)
  }

  it should "EXERCISE 3.29 depthViaFold" in {
    val t =
      Branch(
        Branch(
          Leaf(1),
          Leaf(100)
        ),
        Branch(Leaf(50),
          Branch(
            Leaf(1),
            Leaf(100)
          )
        )
      )
    val ret = Tree.depthViaFold(t)
    assert(ret == 3)
  }

  it should "EXERCISE 3.29 mapViaFold" in {
    val a =
      Branch(
        Branch(
          Leaf(1),
          Leaf(100)
        ),
        Branch(
          Leaf(50),
          Leaf(8)
        )
      )
    val r = Tree.mapViaFold(a)(_.toString)
    val b =
      Branch(
        Branch(
          Leaf("1"),
          Leaf("100")
        ),
        Branch(
          Leaf("50"),
          Leaf("8")
        )
      )
    assert(r == b)
  }
}
