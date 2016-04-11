package fpinscala.testing

import fpinscala.state._

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

trait Prop {
  import Prop._
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  // def &&(p: Prop): = new Prop {
  // def check = Prop.this.check && p.check
  // }

}

// trait Gen[A] {

// def listOf(a: Gen[A]): Gen[List[A]]
// def listOfN(n: Int, a: Gen[A]): Gen[List[A]]
// def forAll(a: Gen[A])(f: A => Boolean): Prop
// }

case class Gen[+A](sample: State[RNG, A]) {

}

object Gen {

  /**
   * EXERCIZE 8.4
   *
   * startからstopExclusiveの範囲内の整数を生成する。
   */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val rng = SimpleRNG(0)
    Gen(State(rng.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
  }

}

