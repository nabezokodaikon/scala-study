package fpinscala.laziness

object LazinessExample {

  // 引数onTrueとonFalseを関数リテラルで渡している。
  def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
    if (cond) onTrue() else onFalse()

  // if2の引数の渡し方の省略表記。
  def if3[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse

  // 引数iを都度評価するため、iを2回評価している。
  def maybeTwice(b: Boolean, i: => Int) =
    if (b) i + i else 0

  // 引数iをキャッシュするため、iは1回しか評価されない。
  def maybeTwice2(b: Boolean, i: => Int) = {
    lazy val j = i
    if (b) j + j else 0
  }

}

