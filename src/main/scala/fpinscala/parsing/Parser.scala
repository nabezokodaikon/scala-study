package fpinscala.parsing

import scala.language.higherKinds

// implicit修飾子を使用するときの警告を抑制。
// implicit修飾子を使用していることを示す。
import scala.language.implicitConversions

trait Prsers[ParserError, Parser[+_]] { self =>
  // このParsersインスタンスをselfという名前で参照する。
  // これはあとからParserOpsで使用される。

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    // selfを使ってtraitのorメソッドを明示的かつ明確に参照。
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
  }
}
