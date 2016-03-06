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
}
