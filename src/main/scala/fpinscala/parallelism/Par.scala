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

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

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

  /**
   * リスト7-15
   *
   * 最初の計算の結果に基づいて、フォークする2つの計算のどちらかを関数に選択させる。
   * condがtrueになる場合はtを実行し、condがfalseになる場合はfを実行します。
   */
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es)
      else f(es)

  /**
   * EXERCISE 7.11
   */
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val ind = run(es)(n).get
      run(es)(choices(ind))
    }

  def choiseViaChoiseN[A](a: Par[Boolean])(ifTrue: Par[A])(ifFalse: Par[A]): Par[A] =
    choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

  /**
   * EXERCISE 7.12
   */
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => {
      val k = run(es)(key).get
      run(es)(choices(k))
    }

  /**
   * EXERCISE 7.13
   */
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val a = run(es)(pa).get
      run(es)(choices(a))
    }

  def choiseViaChooser[A](pa: Par[Boolean])(ifTrue: Par[A])(ifFalse: Par[A]): Par[A] =
    chooser(pa)(a => if (a) ifTrue else ifFalse)

  def choiceNViaChooser[A](pn: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(pn)(n => choices(n))

  def choiceMapViaChooser[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    chooser(key)(k => choices(k))

  /**
   * EXERCIZE 7.14
   */
  def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
    es => {
      val a = run(es)(p).get
      run(es)(f(a))
    }

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(x => x)

  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get())

}

/**
 * EXERCIZE 7.3
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
