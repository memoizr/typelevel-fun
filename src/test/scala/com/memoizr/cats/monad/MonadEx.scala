package com.memoizr.cats.monad

import org.scalatest.{FlatSpecLike, Matchers}

class MonadEx extends FlatSpecLike with Matchers {

  "Monad" should "support a flatten operation" in {
    Option(Option(1)).flatten shouldBe Option(1)
    Option(None).flatten shouldBe None
    List(List(1), List(2, 3)).flatten shouldBe List(1, 2, 3)
  }

  import cats._

  implicit def optionMonad(implicit app: Applicative[Option]) =
    new Monad[Option] {
      override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = app.map(fa)(f).flatten

      override def tailRecM[A, B](a: A)(f: (A) => Option[Either[A, B]]): Option[B] = ???

      override def pure[A](x: A): Option[A] = app.pure(x)
    }

  implicit val listMonad = new Monad[List] {
    override def flatMap[A, B](fa: List[A])(f: (A) => List[B]): List[B] =
      fa.flatMap(f)

    def pure[A](a: A): List[A] = List(a)

    override def tailRecM[A, B](a: A)(f: (A) => List[Either[A, B]]): List[B] = ???

  }

  import cats._
  import cats.implicits._

  "flatMap" should "do the flatMap operation" in {

    Monad[List].flatMap(List(1, 2, 3))(x => List(x, x)) shouldBe List(1, 1, 2, 2, 3, 3)
  }

  "ifM" should "perform operation conditionally based on result of previous operations" in {
    Monad[Option].ifM(Option(true))(Option("truthy"), Option("falsy")) shouldBe Option("truthy")
    Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4)) shouldBe List(1, 2, 3, 4, 1, 2)
  }

  case class OptionT[F[_], A] (value: F[Option[A]])

  implicit def optionTMonad[F[_]](implicit monadInstance: Monad[F]) = {
    new Monad[({type B[C] = OptionT[F,  C]})#B] {
      override def flatMap[A, B](fa: OptionT[F, A])(f: (A) => OptionT[F, B]): OptionT[F, B] = OptionT[F, B] {
        monadInstance.flatMap(fa.value) {
          case None => monadInstance.pure(None)
          case Some(a) => f(a).value
        }
      }

      override def pure[A](x: A): OptionT[F, A] = OptionT(monadInstance.pure(Some(x)))

      override def tailRecM[A, B](a: A)(f: (A) => OptionT[F, Either[A, B]]): OptionT[F, B] = ???
    }
  }

  "composition" should "be possible" in {
    optionTMonad[List].pure(42) shouldBe OptionT(List(Option(42)))

  }
}
