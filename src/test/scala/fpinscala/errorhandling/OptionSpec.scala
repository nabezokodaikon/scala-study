package fpinscala.errorhandling

import org.scalatest.FlatSpec

class OptionSpec extends FlatSpec {

  behavior of "Opiton テスト"

  it should "リスト41" in {
    intercept[Exception] {
      Option.failingFn(12)
    }
  }
}
