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

  // フィボナッチ数を取得します。
  def fib(n: Int): Int = {
    def go(n: Int, a: Int, b: Int): Int = {
      if (n <= 1) a
      else if (n <= 2) b
      else go(n - 1, b, a + b)
    }

    go(n, 0, 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int) =
    s"The ${name} of ${n} is ${f(n)}."

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) =
    s"The factorial of ${n} is ${factorial(n)}."

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
  }
}
