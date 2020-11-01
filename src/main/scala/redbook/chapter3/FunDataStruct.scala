/**
Declare this file as part of the redbook.chapter3 package
 */
package redbook.chapter3

/**
 * List data type is parametrized on a type A
 * + is a variance annotation:
 * it signals that the type param A is a covariant or positive parameter of List
 * That means that for any types X and Y,
 * if X is subtype of Y,
 * then List[X] is also a subtype of List[Y]
 */
sealed trait List[+A]

/**
 * A List data constructor that represents an empty list.
 * It extends Nothing, which is a subtype of ALL types.
 * Extending Nothing in conjunction with the variance annotation "+"
 * allows Nil to be considered a List[Int], a List[Double], etc.
 */
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// List companion object
// Companion objects are a mechanism to enhance a data type, List in this case,
// by adding methods such as sum, product, and apply
object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](lst: List[A], head: A): List[A] = lst match {
    case Nil => Cons(head, Nil)
    case Cons(x, xs) => Cons(head, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else drop(List.tail(l), n - 1)
  }

  //def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
  //}

  def append[A](l1: List[A], a2: List[A]): List[A] = l1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }
}

object ListTestRunner {

  def main(args: Array[String]): Unit = {
    println("Testing My List companion object")
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    println("Testing x is %d".format(x))
    assert(x == 15)
  }
}