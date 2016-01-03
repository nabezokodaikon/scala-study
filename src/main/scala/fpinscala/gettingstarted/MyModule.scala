package fpinscala.gettingstarted

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

  // EXERCIZE 2.2
  // 指定された比較関数に従ってArray[A]がソートされているかどうかを調べる。
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) loop(n + 1)
      else false
    }

    loop(0)
  }

  // EXERCIZE 2.3
  // カリー化
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  // EXERCIZE 2.4
  // 非カリー化
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  // EXERCIZE 2.5
  // 関数合成
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

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
