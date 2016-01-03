package fpinscala.gettingstarted

import org.scalatest.FlatSpec

class MyModuleSpec extends FlatSpec {

  behavior of "フィボナッチ数取得"

  it should "1番目を指定した場合 0 を返す" in {
    assert(MyModule.fib(1) === 0)
  }

  it should "2番目を指定した場合 1 を返す" in {
    assert(MyModule.fib(2) === 1)
  }

  it should "3番目を指定した場合 1 を返す" in {
    assert(MyModule.fib(3) === 1)
  }

  it should "7番目を指定した場合 8 を返す" in {
    assert(MyModule.fib(7) === 8)
  }

}
