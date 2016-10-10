package com.memoizr.cats.functor

import org.scalatest.{FlatSpecLike, Matchers}

class FunctorEx extends FlatSpecLike with Matchers {

  import cats._

//  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
//    override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = fa map f
//  }

  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa map f
  }

  implicit def function1Functor[In]: Functor[({type F[A] = Function1[In, A]})#F] = new Functor[({type F[A] = Function1[In, A]})#F] {
    def map[A, B](fa: In => A)(f: A => B): Function1[In, B] = fa andThen f
  }

  import cats.implicits._

  "functor" should "map over value" in {
    Functor[Option].map(Option("hello"))(_.length) shouldBe Some(5)
    Functor[Option].map(None : Option[String])(_.length) shouldBe None
  }

  "mere functions" should "be lifted to functor transformations" in {
    val lenOption: Option[String] => Option[Int] = Functor[Option].lift(_.length)
    lenOption(Some("hello")) shouldBe Some(5)
  }

  "fproduct" should "pair a value with the result of applying a transformation to that value" in {
    val source = List("Cats", "is", "awesome")
    val product = Functor[List].fproduct(source)(_.length).toMap

    product shouldBe Map("Cats" -> 4, "is" -> 2, "awesome" -> 7)
  }

  "compose" should "compose two functors" in {
    val listOpt = Functor[List] compose Functor[Option]
    listOpt.map(List(Some(1), None, Some(3)))(_+1) shouldBe List(Some(2), None, Some(4))
  }

}
