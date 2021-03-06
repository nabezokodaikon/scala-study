package fpinscala.errorhandling

sealed trait Option[+A] {

  // EXERCIZE 4.1 
  // Option が None 出ない場合は f を適用。
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  // EXERCIZE 4.1 
  // Option が None 出ない場合は、失敗する可能性のある f を適用。
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def flatMap_1[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  // EXERCIZE 4.1 
  // B >: A は、パラメータ B の型が A の型に等しいか、
  // A のスーパークラスでなければならないことを示す。
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  // EXERCIZE 4.1 
  // 必要でない限り、ob を評価しない。
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def orElse_1[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  // EXERCIZE 4.1 
  // 値が f の条件を満たさない場合は、Some を None に変換。
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  /** EXERCIZE 4.2 */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

  def absO: Option[Double] => Option[Double] = lift(math.abs)

  /** EXERCIZE 4.3 */
  def map2[X, Y, Z](a: Option[X], b: Option[Y])(f: (X, Y) => Z): Option[Z] = (a, b) match {
    case (Some(aa), Some(bb)) => Some(f(aa, bb))
    case _ => None
  }

  def map2_1[X, Y, Z](a: Option[X], b: Option[Y])(f: (X, Y) => Z): Option[Z] =
    a.flatMap(aa =>
      b.map(bb =>
        f(aa, bb)))

  def map2ViaFor[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  /** EXERCIZE 4.4 */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case h :: t => h.flatMap(hh => sequence(t).map(tt => hh :: tt))
      case Nil => Some(Nil)
    }

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

  /** EXERCIZE 4.5 */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => f(h).flatMap(hh => traverse(t)(f).map(hh :: _))
    }

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse_1(t)(f))(_ :: _)
    }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

}
