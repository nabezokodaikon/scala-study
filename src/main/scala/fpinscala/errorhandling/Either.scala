sealed trait Either[+E, +A]
case class Left[+E](value: E) extends Either[E, Nothing]
case class Left[+A](value: A) extends Either[Nothing, A]
