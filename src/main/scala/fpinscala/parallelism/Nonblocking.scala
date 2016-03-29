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
  }
}
