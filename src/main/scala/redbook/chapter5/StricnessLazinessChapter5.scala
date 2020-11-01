package redbook.chapter5

import scala.List
import redbook.chapter5.Stream.{cons, empty}

sealed trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => List()
    case Cons(hd, tl) => hd() :: tl().toList
  }

  def headOption: Option[A] = this match {
    case Empty  => scala.None
    case Cons(h, _) => Some(h())
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(hd, tl) if n > 1 => Stream.cons(hd(), tl().take(n - 1))
    case Cons(hd, _) if n == 1 => Stream.cons(hd(), Stream.empty)
    case _ => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, tl) if n > 0 => tl().drop(n - 1)
    case _ => this
  }

  // Difference between takeWhile and filter
  // filter will remove all items from the stream that do not satisfy the condition.
  // takeWhile will abort the stream on the first occurrence of an item which does not satisfy the condition.
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(hd, tl) if f(hd()) => Stream.cons(hd(), tl().takeWhile(f))
    case _ => Stream.empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def forAll(f: A => Boolean): Boolean = this match {
    case Cons(h, t) => f(h()) || t().forAll(f)
    case _ => false
  }

  def headOption2: Option[A] = {
    Stream.foldRight[A, Option[A]](this)(scala.None)((h, _) => Some(h))
  }

  def append[B >: A](other: Stream[B]): Stream[A] = {
    Stream.foldRight(other: Stream[B])(this: Stream[A]){
      (head: B, tail: Stream[A]) => this.append(cons(head, tail))
    }
  }

  def filter(f: A => Boolean): Stream[A] = {
    Stream.foldRight(st = this)(empty[A]){
      (h, t) => if (f(h)) cons(h, t) else t
    }
  }

  def map[B](f: A => B): Stream[B] = {
    // this
    // z default case
    // Last one: folding function
    Stream.foldRight(this)(empty[B])((h, t) => cons(f(h), t))
  }

  def flatMap[B](f: A => B): Stream[B] = {
    Stream.foldRight(this)(empty[B]) {
      (head, tail) => cons(f(head), tail)
    }
  }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
  // Smart constructor to create nonempty streams
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  // Smart constructor to build empty streams of a specific type
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def foldRight[A, B] (st: Stream[A]) (z : B) (f: (A, B) => B): B = {
    st match {
      case Cons(h, t) => f(h(), foldRight(t())(z)(f))
      case _ => z
    }
  }

  def ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a: A, constant(a))

  def constant2[A](a: A): Stream[A] = {
    // This needs forAll logic to be changed to an &&
    // because of short-circuiting
    lazy val stream: Stream[A] = Stream.cons(a, stream)
    stream
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def loop(current: Int, next: Int): Stream[Int] = {
      cons(current, loop(next, current + next))
    }
    loop(0, 1)
  }

}
