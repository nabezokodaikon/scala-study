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

// テストケースの1つがプロパティを反証したことを示す。
case class Falsified(
    failure: FailedCase,
    success: SuccessCount
) extends Result {
  def isFalsified = true
}

case class Prop(run: (TestCases, RNG) => Result)

object Prop {

  // バグを引き起こすテストケース。
  type FailedCase = String

  // 成功するテストケースの数。
  type SuccessCount = Int

  // テストケースの数。
  type TestCases = Int

  // リスト 8-3
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    Prop { (n, rng) =>
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

}

// trait Gen[A] {
// // def map[A,B](f: A => B): Gen[B] = ???
// // def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
// }

trait SGen[+A] {

}

