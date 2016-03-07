package fpinscala.errorhandling

import org.scalatest.FlatSpec

class EitherSpec extends FlatSpec {

  behavior of "Either テスト"

  it should "mean" in {
    assert(Either.mean(List[Double]()) == Left("mean of empty list!"))
    assert(Either.mean(List(1.0, 2)) == Right(1.5))
  }

  it should "safeDiv" in {
    assert {
      Either.safeDiv(4, 2) match {
        case Left(e) => false
        case Right(v) => true
      }
    }
    assert {
      Either.safeDiv(4, 0) match {
        case Left(e) => true
        case Right(v) => false
      }
    }
  }

  it should "EXERCIZE 4.6 map" in {
    assert {
      Left(new Exception("")).map(i => i) match {
        case Left(e) => true
        case _ => false
      }
    }
    assert(Right(1).map(i => i + 1) == Right(2))
  }

  it should "EXERCIZE 4.6 flatMap" in {
    assert {
      Left(new Exception("")).flatMap(i => i) match {
        case Left(e) => true
        case _ => false
      }
    }
    assert(Right(1).flatMap(i => Either.Try(i + 1)) == Right(2))
  }

  it should "EXERCIZE 4.6 orElse" in {
    assert(Left(new Exception("")).orElse(Right(1)) == Right(1))
    assert(Right(2).orElse(Right(1)) == Right(2))
  }

  it should "EXERCIZE 4.6 map2" in {
    val a = Right(100)
    val b = Right(0.5)
    val r1 = a.map2(b)(_ * _)
    val r2 = a.map2(b)(_ * _)
    assert(r1 == Right(50.0))
    assert(r1 == r2)
  }

  it should "EXERCIZE 4.7 traverse" in {
    val as = List("1", "2", "3")
    val ar = Either.traverse(as)(a => Either.Try(a.toInt))
    assert(ar == Right(List(1, 2, 3)))
    val bs = List("1", "a", "b")
    val br = Either.traverse(bs)(a => Either.Try(a.toInt))
    assert {
      br match {
        case Left(e) => true
        case _ => false
      }
    }
  }

  it should "EXERCIZE 4.7 sequence" in {
    val as = List(Right(1), Right(2), Right(3))
    val ar = Either.sequence(as)
    assert(ar == Right(List(1, 2, 3)))
    val bs = List(Right(1), Left(new Exception("error")), Right(3))
    val br = Either.sequence(bs)
    assert {
      br match {
        case Left(e) => true
        case _ => false
      }
    }
  }
}
