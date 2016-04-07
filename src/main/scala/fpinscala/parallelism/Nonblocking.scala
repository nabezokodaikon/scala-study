package fpinscala.parallelism

import java.util.concurrent.{ Callable, CountDownLatch, ExecutorService }
import java.util.concurrent.atomic.AtomicReference
import language.implicitConversions

object Nonblocking {

  trait Future[+A] {
    // 同一パッケージ内のコードからしかアクセスできない記述方法。
    private[parallelism] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def run[A](es: ExecutorService)(p: Par[A]): A = {

      // 結果を格納するためのスレッドセーフなミュータブル参照。
      val ref = new AtomicReference[A]

      // countDownメソッドが特定の回数呼び出されるまでスレッドを待機させることができる。
      val latch = new CountDownLatch(1)

      // 値を取得したら、結果を設定し、latchを解放。
      p(es) { a =>
        ref.set(a);
        latch.countDown
      }

      // 結果の準備が整い、latchが解放されるまで待機。
      latch.await

      // latchを渡した後、refが設定されていることはわかっているため、その値を返す。
      ref.get
    }

    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          // evalは、aの評価をフォークし、すぐに制御を戻す。
          // コールバックは別のスレッドで非同期に呼び出される。
          eval(es)(a(es)(cb))
      }

    // ExecutorServiceを使ってアクションを非同期で実行するヘルパー関数。
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })

    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          // 2つの結果を格納するためのミュータブルな変数。
          var ar: Option[A] = None
          var br: Option[B] = None

          // 両方の結果を待機し、fを使ってそれらを結合し、結果をcbに渡すアクター。
          val combiner = Actor[Either[A, B]](es) {
            // Aの結果のほうが先に渡された場合は、
            // それをarに格納してBの結果を待つ。
            // Aの結果のほうが後に渡され、Bがすでにある場合は、
            // 両方の結果でfを呼び出し、結果として得られたCをコールバックcbに渡す。
            case Left(a) => br match {
              case None => ar = Some(a)
              case Some(b) => eval(es)(cb(f(a, b)))
            }
            // Bの結果のほうが先に渡された場合は、
            // それをbrに格納してAを待つ。
            // Bの結果のほうが後に渡され、Aがすでにある場合は、
            // 両方の結果でfを呼び出し、結果として得られたCをコールバックcbに渡す。
            case Right(b) => ar match {
              case None => br = Some(b)
              case Some(a) => eval(es)(cb(f(a, b)))
            }
          }

          // アクターをコールバックとして両側に渡す。
          // A側では結果をLeftでラッピングし、B側ではRightでラッピングする。
          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }

    def map[A, B](p: Par[A])(f: A => B): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => eval(es) { cb(f(a)) })
      }

    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
      sequence(as.map(asyncF(f)))

  }
}
