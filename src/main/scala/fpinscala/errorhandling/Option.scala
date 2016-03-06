package fpinscala.errorhandling

sealed trait Option[+A] {
  // Option が None 出ない場合は f を適用。
  def map[B](f: B => A): Option[B]

  // Option が None 出ない場合は、失敗する可能性のある f を適用。
  def flatMap[B](f: A => Option[B]): Option[B]

  // B >: A は、パラメータ B の型が A の型に等しいか、
  // A のスーパークラスでなければならないことを示す。
  def getOrElse[B >: A](default: => B):B

  // 必要でない限り、ob を評価しない。
  def orElse[B >: A](ob: => Option[B]): Option[B]

  // 値が f の条件を満たさない場合は、Some を None に変換。
  def filter(f: A => Boolean):Option[A]
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

}
