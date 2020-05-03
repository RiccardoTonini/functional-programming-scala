/* sealed trait Option[+A] */
/*import scala.{Option => _, Either => _, _}*/
import math

sealed trait Option[+A] {

  case class Some[+A] (get: A) extends Option[A]
  case object None extends Option[Nothing]

  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(a) => Option[B]
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  /* don't eval unless needed */
  def OrElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }
}

object MathUtil {

  def mean(xs: Seq[Double]): Option[Double] = match {
    case Nil => None
    case _ => Some(xs.sum.toDouble / xs.size)
  }

  def variance(xs: Seq[Double]): Option[Double] = {

  }
}
