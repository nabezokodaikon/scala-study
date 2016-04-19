package fpinscala.state

/**
 * リスト 6-2
 */
trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  /**
   * リスト 6-3
   */
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  /**
   * EXERCISE 6.1
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  /**
   * EXERCISE 6.2
   */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  /**
   * EXERCISE 6.3
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /**
   * EXERCISE 6.4
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(c: Int, r: RNG, xs: List[Int]): (List[Int], RNG) = {
      if (c <= 0) {
        (xs, r)
      } else {
        val (x, r2) = r.nextInt
        go(c - 1, r2, x :: xs)
      }
    }
    go(count, rng, List())
  }

  /**
   * リスト 6-4
   *
   * 状態アクションデータ型であるRNGの型エイリアス。
   */
  type Rand[+A] = RNG => (A, RNG)

  /**
   * リスト 6-5
   */
  val int: Rand[Int] = _.nextInt

  /**
   * リスト 6-6
   */
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  /**
   * リスト 6-7
   */
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // 2で割り切れる0以上のIntを生成します。
  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  /**
   * EXERCISE 6.5
   */
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  /**
   * EXERCISE 6.6
   */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def intDoubleViaMap2: Rand[(Int, Double)] = both(int, doubleViaMap)

  def doubleIntViaMap2: Rand[(Double, Int)] = both(doubleViaMap, int)

  /**
   * EXERCISE 6.7
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft(unit(List[A]()))((xs, h) => map2(h, xs)(_ :: _))

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    rng => {
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        (mod, rng2)
      else nonNegativeLessThan(n)(rng)
    }

  /**
   * EXERCISE 6.8
   */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }

  def nonNegativeLessThanViaFlatMap(count: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % count
      if (i + (count - 1) - mod >= 0)
        unit(mod)
      else nonNegativeLessThanViaFlatMap(count)
    }

  /**
   * EXERCISE 6.9
   */
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a, b)))

  def rollDie: Rand[Int] = nonNegativeLessThanViaFlatMap(6)
}

/**
 * EXERCISE 6.10
 */
case class State[S, +A](run: S => (A, S)) {
  import State._

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s1 => {
      val (a, s2) = run(s1)
      f(a).run(s2)
    })
}

object State {

  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldLeft(unit[S, List[A]](List()))((xs, h) => h.map2(xs)(_ :: _))

  val int: Rand[Int] = State[RNG, Int](RNG.map(RNG.nonNegativeLessThanViaFlatMap(10))(_ + 1))

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  val ns1: Rand[List[Int]] = {
    int.flatMap(x => // intはランダムな整数を1つ生成するRand[Int]型の値。 
      int.flatMap(y =>
        ints(x).map(xs => // ints(x)は長さxのリストを生成。
          xs.map(_ % y)))) // リスト内のすべての要素をyで割った余りと置き換える。
  }

  val ns2: Rand[List[Int]] = for {
    x <- int
    y <- int
    xs <- ints(x)
  } yield xs.map(_ % y)

  // リスト 6-10
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // 現在の状態を取得し、sに代入。 
    _ <- set(f(s)) // sに適用されるfに新しい状態を設定。
  } yield ()

  // リスト 6-11
  def get[S]: State[S, S] = State(s => (s, s))

  // リスト 6-12
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

