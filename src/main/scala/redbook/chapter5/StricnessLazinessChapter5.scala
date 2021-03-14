package redbook.chapter5

import redbook.chapter2.FunTools.{compose, curry, partial1, uncurry}

import scala.List
import redbook.chapter5.Stream.{cons, empty, foldRight}

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
    // z default case
    // Last one: folding function
    Stream.foldRight(this)(empty[B])((h, t) => cons(f(h), t))
  }

  def flatMap[B](f: A => B): Stream[B] = {
    // empty[B] z default case
    // Last one: folding function
    Stream.foldRight(this)(empty[B]){
      (head, tail) => f(head).asInstanceOf[Stream[B]] append tail
    }
  }

  def unfoldMap[B](f: A => B): Stream[B] =
    // f: S => Option[(B, S)]
    Stream.unfold(this: Stream[A]) {
      {
        // Check @ to avoid deconstructing
        // case cons @ Cons(h,t) => cons
        case Cons(head, tail) => Some(f(head()), tail())
        case _ => None
        // case `empty` => None   // symbol
      }
    }

  def unfoldTake(n: Int): Stream[A] = {
    // unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A]
    Stream.unfold((this, n)) {
      {
        case (Cons(hd, tl), n) if n > 1 => Some((hd(), (tl(), n - 1)))
        case (Cons(hd, tl), n) if (n == 1) => Some((hd(), (empty, 0)))
        case _ => None
      }
    }: Stream[A]
  }

  def unfoldTakeWhile(f: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(hd, tl) if f(hd()) => Some((hd(), tl()))
      case _ => None
    }

  def zipWith[B, C](streamB: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, streamB)) {
      case (Cons(hdA, tlA), Cons(hdB, tlB)) => Some((f(hdA(), hdB()), (tlA(), tlB())))
      case _ => None
    }

  def zipAll[B](other: Stream[B]): Stream[(Option[A],Option[B])] =
    Stream.unfold((this, other)) {
      case (Cons(hd1, tl1), Cons(hd2, tl2)) => Some(((Some(hd1()), Some(hd2())), (tl1(), tl2())))
      case (Cons(hd1, tl1), Empty) => Some(((Some(hd1()), None), (tl1(), empty)))
      case (Empty, Cons(hd2, tl2)) => Some(((None, Some(hd2())), (empty, tl2())))
      case _ => None
    }

  /*
  5.14 Hard: Implement startsWith using functions you’ve written. It should check
  if one Stream is a prefix of another. For instance,
  Stream(1,2,3) startsWith Stream(1,2) would be true.
   */
  def startsWith[B >: A](other: Stream[B]): Boolean = (this, other) match {
    case (Cons(hd, tl), Cons(otherHd, otherTl)) if hd() == otherHd() => tl().startsWith(otherTl())
    case (_, Empty) => true
    case _ => false
  }

  def startsWith2[A](other: Stream[A]): Boolean =
    zipAll(other).takeWhile(element => !element._2.isEmpty) forAll {
      case (hd, hd2) => hd == hd2
    }

  def startsWith3[A](other: Stream[A]): Boolean = {
    zipAll(other).takeWhile {
      case (_, b) => b != None
    } forAll {
      case (hd, hd2) => hd == hd2
    }
  }

  // 15.5 Implement tails using unfold. For a given Stream, tails returns the Stream
  // of suffixes of the input sequence, starting with the original Stream. For
  // example, given Stream(1,2,3), it would return
  // Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).
  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case s => Some((s, s.drop(1)))
    } append Stream(empty)

  def tails2: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case stream @ Cons(_, tl) => Some((stream, tl()))
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  // 5.6 Generalize tails to the function scanRight,
  // which is like a foldRight that returns a stream of
  // the intermediate results.
  // - foldRight
  // - recur how to reuse stream of intermediate results
  // - How to cache previously computed intermediate results?
  // Stream(1, 2 , 3).scanRight(0)(_ + _).toList
  // res: List[Int] = List(6, 5, 3, 0)
  // List(1 + 2 + 3 + 0, 2 + 3 + 0, 3 + 0, 0)
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    val (_, accumulator) = foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })
    accumulator
  }

  /**
   * Attempt recursive implementation of scanRight
  def scanRight2[B](init: B)(f: (A, => B) => B): Stream[B] = {
    this match {
      case Empty => Stream(init)
      case Cons(head, tail) =>
        val intermediate: Stream[B] = tail().scanRight(init)(f)
    }
  }
  **/

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    // The arrow `=>` in front of the argument type `B` indicates
    // that the function `f` takes its second argument by name
    // and may choose not to evaluate it.
    // If `f` doesn't evaluate its second argument,
    // the recursion never occurs.
    this match {
      case Cons(head, tail) => f(head(), tail().foldRight(z)(f))
      case _ => z
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

  /*
  More general stream-building function called unfold.
  It takes an initial state and a function to produce both the
  next state and the next value in the generated stream.
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    /* z Initial value */
    val next_state_value_pair = f(z)
    next_state_value_pair match {
      case None => empty
      case Some((current_val, next_state)) => cons(current_val, unfold(next_state)(f))
    }

  }

  def unfoldOnes: Stream[Int] = {
    // Next val and state
    def nextValState(z: Int) = {
      Some((1, 1))
    }
    unfold(1)(nextValState)
  }

  def unfoldFrom(n: Int): Stream[Int] = {
    // Next val and state
    def nextValState(n: Int) = {
      Some((n, n + 1))
    }
    unfold(n)(nextValState)
  }

  def unfoldConstant[A](a: A): Stream[A] = {
    // Stream.cons(a: A, constant(a))
    /*
    def nextValState(aValue: A) = {
      Some((aValue, aValue))
    }

    */
    unfold(a)(aValue => Some((aValue, aValue)))
  }
}

object PlayGround {
  def main(args: Array[String]): Unit = {
    println("********   Testing Scan Right   ********")
    val res = Stream(1, 2, 3).scanRight(0)(_ + _).toList
    print(s"Result ${res}")

  }

}
