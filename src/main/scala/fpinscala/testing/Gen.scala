package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ Executors, ExecutorService }

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

// trait Prop {

// def check: Either[(FailedCase, SuccessCount), SuccessCount]

// // EXERCIZE 8.3
// // def &&(p: Prop): Prop = new Prop {
// // def check = Prop.this.check && p.check
// // }

// }

sealed trait Result {
  def isFalsified: Boolean
}

// 全てのテストにパスしたことを示す。
case object Passed extends Result {
  def isFalsified = false
}

case object Proved extends Result {
  def isFalsified = false
}

// テストケースの1つがプロパティを反証したことを示す。
case class Falsified(
    failure: FailedCase,
    success: SuccessCount
) extends Result {
  def isFalsified = true
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  // EXERCIZE 8.9
  def &&(p: Prop) = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Passed | Proved => p.run(max, n, rng)
        case x => x
      }
  }

  def ||(p: Prop) = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
        case x => x
      }
  }

  def tag(msg: String) = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
  }
}

object Prop {

  // バグを引き起こすテストケース。
  type FailedCase = String

  // 成功するテストケースの数。
  type SuccessCount = Int

  // テストケースの数。
  type TestCases = Int

  // テストケースの最大サイズ
  type MaxSize = Int

  // リスト 8-3
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        // ペア(a, i)のストリーム。
        // aはランダム値、iはストリーム内でのそのインデックス。
        case (a, i) =>
          try {
            if (f(a)) {
              Passed
            } else {
              // テストに失敗した場合は、失敗したケースとそのインデックスを記録し、
              // それまでに成功したテストの数がわかるようにする。
              Falsified(a.toString, i)
            }
          } catch {
            // テストケースが例外を生成した場合は、それを結果に記録。
            case e: Exception => Falsified(buildMsg(a, e), i)
          }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  // ジェネレータを繰り返しサンプリングすることにより、Aの無限ストリームを生成。
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"nenerated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

case class Gen[A](sample: State[RNG, A]) {

  // EXERCIZE 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => this.listOfN(n))

  // EXERCIZE 8.10
  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {

  // EXERCIZE 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def choose2(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(rng => RNG.nonNegativeInt(rng) match {
      case (n, rng2) => (start + n % (stopExclusive - start), rng2)
    }))

  // EXERCIZE 8.5
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  // EXERCIZE 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(a => if (a) g1 else g2)

  // EXERCIZE 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)) = {
    // 閾値を算出する。
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double).flatMap(d =>
      if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  // EXERCIZE 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

}

case class SGen[+A](forSize: Int => Gen[A]) {

  // EXERCIZE 8.11
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] =
    SGen { forSize(_).map(f) }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val forSize2: Int => Gen[B] =
      n => {
        forSize(n).flatMap { f(_).forSize(n) }
      }
    SGen(forSize2)
  }

  def **[B](s2: SGen[B]): SGen[(A, B)] =
    SGen(n => apply(n) ** s2(n))
}

