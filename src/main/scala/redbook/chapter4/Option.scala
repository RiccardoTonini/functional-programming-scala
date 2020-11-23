package redbook.chapter4

import scala.util.Try
// Hide std library `Option`, `Some` and `Either`
// since we are writing our own in this chapter
import scala.{Either => _, Option => _, Some => _}

sealed trait Option[+A] {

  case class Some[+A] (get: A) extends Option[A]
  case object None extends Option[Nothing]

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
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
    case Some(a) => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

  def isEmpty[B]: Boolean = this match {
    case None => true
    case _ => false
  }
}

case class Some[+A] (get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    def isNone(a: Option[A]): Boolean = a match {
      case None => true
      case _ => false
    }
    val filteredAs: List[Option[A]] = as.filter{isNone}
    filteredAs match {
      case Nil => Some(as.map{ a => a.asInstanceOf[A]})
      case _ => None
    }
  }

  def sequence2[A](l: List[Option[A]]): Option[List[A]] = l match {
    case Nil => Some(Nil)
    case h :: t => h match {
      case None => None
      case Some(head) => sequence2(t) match {
        case None => None
        case Some(list) => Some(head :: list)
      }
    }
  }

  def sequence3[A](as: List[Option[A]]): Option[List[A]] = {
    as.partition(_.isEmpty) match {
      case (Nil, Nil) => None
      case (Nil, a)   => Some(a.map{ a => a.asInstanceOf[A]})
      case _          => None
    }
  }

  /*
   Sometimes we want to map over a list using a function
   that might fail, returning None if applying it to any
   element of the list fails.
   */
  def parseIntsInefficient(a: List[String]): Option[List[Int]] = {
    val optionalAs: List[Option[Int]] = a.map{
      i => Try(i.toInt).asInstanceOf[Option[Int]]
    }
    sequence(optionalAs)
  }
  /*
  Unfortunately, the above is inefficient because of 2 passes:
  1) convert each String to Option[Int]
  2) combine each Option[Int] value into an Option[List[Int]]
  Wanting to sequence the results of a map this way is common, so
  let's write a new generic function, traverse
   */
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    val bs: List[B] = as.flatMap{a: A => f(a).asInstanceOf[IterableOnce[B]]}
    bs.asInstanceOf[Option[List[B]]]
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  def traverseRecursive[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h::t => map2(f(h), traverseRecursive(t)(f))(_ :: _)
    }

  def parseInts(a: List[String]): Option[List[Int]] = {
    traverse(a)(i => Try(i.toInt).asInstanceOf[Option[Int]])
  }

  def sequenceTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  /*
  def main(args: Array[String]): Unit = {
    println("******************* Test Sequence 1  ******************")
    val test1 = List(Option(1), None)

    val res1 = sequence(test1)

    assert(res1 == None)

    val test2 = List(Option(1), Option(2))
    val expected = Option(List(1, 2))
    val res2 = sequence(test2)

    assert(res2 == expected)

    println("******************* Test Sequence 2  ******************")
    val test3 = List(Option(1), None)

    val res3 = sequence2(test3)

    assert(res3 == None)

    val test4 = List(Option("a"), Option("b"))
    val expectedRes = Option(List("a", "b"))
    val res4 = sequence2(test4)

    assert(res4 == expectedRes)

  }
   */

}

object MathUtil {

  def mean(xs: Seq[Double]): Option[Double] = {
    xs match {
      case Nil => None
      case _ => Some(xs.sum.toDouble / xs.size)
    }
  }

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap{
      m => mean(xs.map{
        x => math.pow(x - m, 2)
      })
    }
}
