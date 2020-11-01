//package redbook.Chapter4
//// import scala.{Either => _, Option => _, _}
//import scala.util.Try
//
//object Option {
//
//  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
//    def isNone(a: Option[A]): Boolean = a match {
//      case None => true
//      case _ => false
//    }
//    as.filter{isNone} match {
//      case Nil => Option(as.flatten)
//      case _ => None
//    }
//  }
//
//  def sequence2[A](as: List[Option[A]]): Option[List[A]] = {
//    as.partition(_.isEmpty) match {
//      case (Nil, Nil) => None
//      case (Nil, x)   => Some(x.map(_.get))
//      case _          => None
//    }
//  }
//
//  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
//    Option(as.flatMap{a:A => f(a)})
//  }
//
//  def parseInts(a: List[String]): Option[List[Int]] = {
//    traverse(a)(i => Try(i.toInt).toOption)
//  }
//
//  def main(args: Array[String]): Unit = {
//      println("******************* Test Sequence 1  ******************")
//      val test1 = List(Option(1), None)
//
//      val res1 = sequence(test1)
//
//      assert(res1 == None)
//
//      val test2 = List(Option(1), Option(2))
//      val expected = Option(List(1, 2))
//      val res2 = sequence(test2)
//
//      assert(res2 == expected)
//
//      println("******************* Test Sequence 2  ******************")
//      val test3 = List(Option(1), None)
//
//      val res3 = sequence2(test3)
//
//      assert(res3 == None)
//
//      val test4 = List(Option("a"), Option("b"))
//      val expectedRes = Option(List("a", "b"))
//      val res4 = sequence2(test4)
//
//      assert(res4 == expectedRes)
//
//  }
//
//}