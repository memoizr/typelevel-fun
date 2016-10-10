package com.memoizr.cats.apply

import javax.xml.crypto.dsig.Transform

import org.scalatest.{FlatSpecLike, Matchers}

class ApplyEx extends FlatSpecLike with Matchers {

  import cats._
  import cats.implicits._

  //  implicit val optionApply: Apply[Option] = new Apply[Option] {
  //    override def ap[A, B](f: Option[A => B])(fa: Option[A]): Option[B] =
  //      fa.flatMap(a => f.map(ff => ff(a)))
  //
  //    override def map[A, B](subject: Option[A])(transformation: A => B): Option[B] =
  //      subject map transformation
  //
  //    override def product[A, B](firstFunctor: Option[A], secondFunctor: Option[B]): Option[(A, B)] =
  //      for {
  //        a <- firstFunctor
  //        b <- secondFunctor
  //      } yield (a, b)
  //
  //    //      firstFunctor.flatMap(a => secondFunctor.map(b => (a, b)))
  //  }

  implicit val listApply: Apply[List] = new Apply[List] {
    override def ap[A, B](transformation: List[A => B])(subject: List[A]): List[B] = subject.flatMap(a => transformation.map(ff => ff(a)))

    override def map[A, B](subject: List[A])(transformation: A => B): List[B] =
      subject map transformation

    override def product[A, B](firstFunctor: List[A], secondFunctor: List[B]): List[(A, B)] =
      firstFunctor zip secondFunctor
  }

  val intToString: Int => String = _.toString
  val double: Int => Int = _ * 2
  val addTwo: Int => Int = _ + 2

  "map" should "still work" in {
    Apply[Option].map(Some(1))(intToString) shouldBe Some("1")
    Apply[Option].map(Some(1))(double) shouldBe Some(2)
    Apply[Option].map(None)(addTwo) shouldBe None
  }

  "compose" should "behave in the same way as compose does for functors, obviously" in {
    val listOpt = Apply[List] compose Apply[Option]
    val plusOne = (x: Int) => x + 1
    listOpt.ap(List(Some(plusOne)))(List(Some(1), None, Some(3))) shouldBe List(Some(2), None, Some(4))
  }

  "ap" should "not be present in Functor" in {
    Apply[Option].ap(Some(intToString))(Some(1)) shouldBe Some("1")
    Apply[Option].ap(Some(double))(Some(1)) shouldBe Some(2)
    Apply[Option].ap(Some(double))(None) shouldBe None
    Apply[Option].ap(None)(Some(1)) shouldBe None
    Apply[Option].ap(None)(None) shouldBe None
  }

  val addArity2 = (a: Int, b: Int) => a + b
  val addArity3 = (a: Int, b: Int, c: Int) => a + b + c

  "ap with different arities" should "perform the respective operation" in {
    Apply[Option].ap2(Some(addArity2))(Some(1), Some(2)) shouldBe Some(3)
    Apply[Option].ap2(Some(addArity2))(Some(1), None) shouldBe None
    Apply[Option].ap2(Some((_: String) + (_: String)))(Some("foo"), Some("bar")) shouldBe Some("foobar")

    Apply[Option].ap3(Some(addArity3))(Some(1), Some(2), Some(3)) shouldBe Some(6)
  }

  "map with higher arity" should "perform the respective operation" in {
    Apply[Option].map2(Some(1), Some(2))(addArity2) shouldBe Some(3)
    Apply[Option].map3(Some(1), Some(2), Some(3))(addArity3) shouldBe Some(6)
  }

  "tupleN" should "work in a similar way" in {
    Apply[Option].tuple2(Some(1), Some(2)) shouldBe Some((1,2))
    Apply[Option].tuple3(Some(1), Some(2), Some(3)) shouldBe Some((1,2,3))
  }

  "spaceship operator" should "provide an alternative syntax to higher-arity operations" in {
    val option2 = Option(1) |@| Option(2)
    val option3 = option2 |@| Option.empty[Int]

    option2 map addArity2 shouldBe Some(3)
    option3 map addArity3 shouldBe None

    option2 apWith Some(addArity2) shouldBe Some(3)
    option3 apWith Some(addArity3) shouldBe None

    option2.tupled shouldBe Some((1,2))
    option3.tupled shouldBe None
  }
}
