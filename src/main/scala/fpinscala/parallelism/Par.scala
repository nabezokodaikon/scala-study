package fpinscala.parallelism

import java.util.concurrent.TimeUnit

trait Par {

  /**
   * 直ちにa値が得られる計算を作成。
   * 定数値を並列計算に昇格させる。
   */
  // def unit[A](a: A): Par[A]

  /**
   * 2つの並列計算の結果を2項関数で結合する。
   */
  // def map2[A, B, C](a: => Par[A], b: => Par[B])(f: (A, B) => C): Par[C]

  /**
   * runによる並列評価の対象としてマーク。
   * 計算を並列評価の対象としてマークする。
   * この評価はrunによって強制されるまで実際には発生しない。
   */
  // def fork[A](a: => Par[A]): Par[A]

  /**
   * 式aをrunによる並列評価のためのラッピング。
   * 評価されていない引数をParでラッピングし、
   * 並列評価の対象としてマークする。
   */
  // def lazyUnit[A](a: => A): Par[A] =
  // fork(unit(a))

  /**
   * 与えられたParを完全に評価し、forkによって要求される並列計算を生成し、結果の値を取得。
   * 実際に計算を行うことで、Parから値を取得する。
   */
  // def run[A](a: Par[A]): A

}

class ExecutorService {
  def submit[A](a: Callable[A]): Future[A]
}

trait Callable[A] {
  def call: A
}

trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  def unit[A](a: A): Par[A] =
    (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  /**
   * EXECIZE 7.3
   */
  def map2WithTimeout[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      Map2Future(af, bf, f)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

}

/**
 * EXECIZE 7.3
 */
case class Map2Future[A, B, C](
    a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {

  @volatile
  var cache: Option[C] = None
  def isDone = cache.isDefined
  def isCancelled = a.isCancelled || b.isCancelled
  def cancel(evenIfRunning: Boolean) =
    a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
  def get = compute(Long.MaxValue)
  def get(timeout: Long, units: TimeUnit): C =
    compute(TimeUnit.NANOSECONDS.convert(timeout, units))

  private def compute(timeoutInNanos: Long): C = cache match {
    case Some(c) => c
    case None =>
      val start = System.nanoTime
      val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
      val stop = System.nanoTime
      val aTime = stop - start
      val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
      val ret = f(ar, br)
      cache = Some(ret)
      ret
  }
}
