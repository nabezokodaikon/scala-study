package gettingstarted

object MyModule {

  // 絶対値を取得します。
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  // 階乗を取得します。
  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
  }
}
