package redbook.chapter4

sealed trait Either[+E, +A] {
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Left2[+E](value: List[E]) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(a) => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this.flatMap{a: A => b.flatMap{bb => f(a, bb).asInstanceOf[Either[EE, C]]}}
  }

  def traverse[E,A,B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil => Right(Nil)
      case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
    }

  def sequence[E,A](as: List[Either[E,A]]): Either[E, List[A]] =
    traverse(as)(x => x)

  def anotherMap3[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Right(a), Right(b)) => Right(f(a, b))
    case (Left(a), _) => Left(a)
    case (_, Left(b)) => Left(b)
  }

  def map22[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Right(a), Right(b)) => Right(f(a, b))
    case (Left(a), Left(b)) => Left2(List(a, b))
    case (Left(a), _) => Left(a)
    case (_, Left(b)) => Left(b)
  }

}
