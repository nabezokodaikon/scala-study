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

trait Prop {

  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  // EXERCIZE 8.3
  // def &&(p: Prop): Prop = new Prop {
  // def check = Prop.this.check && p.check
  // }

}

object Prop {

  type FailedCase = String
  type SuccessCount = Int

  // def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???

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

