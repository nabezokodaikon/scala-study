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
}
