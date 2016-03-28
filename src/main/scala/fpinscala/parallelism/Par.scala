package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

object Par {

  type Par[A] = ExecutorService => Future[A]

  /**
   * 与えられたParを完全に評価し、forkによって要求される並列計算を生成し、結果の値を取得。
   * 実際に計算を行うことで、Parから値を取得する。
   */
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  /**
   * 直ちにa値が得られる計算を作成。
   * 定数値を並列計算に昇格させる。
   */
  def unit[A](a: A): Par[A] =
    (es: ExecutorService) => UnitFuture(a)

  /**
   * 式aをrunによる並列評価のためのラッピング。
   * 評価されていない引数をParでラッピングし、
   * 並列評価の対象としてマークする。
   */
  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  /**
   * runによる並列評価の対象としてマーク。
   * 計算を並列評価の対象としてマークする。
   * この評価はrunによって強制されるまで実際には発生しない。
   */
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  /**
   * 2つの並列計算の結果を2項関数で結合する。
   */
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

  /**
   * EXECIZE 7.4
   *
   * 任意の関数 A => B から、その結果を非同期で評価する関数へ変換する。
   */
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit[B](f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  /**
   * EXECIZE 7.5
   */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  /**
   * EXECIZE 7.6
   */
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = l.map(asyncF((a: A) => {
      if (f(a)) List(a)
      else List()
    }))
    map(sequence(pars))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

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
