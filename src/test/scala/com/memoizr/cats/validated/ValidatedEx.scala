package com.memoizr.cats.validated

import org.scalatest.{FlatSpecLike, Matchers}
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.{Semigroup, SemigroupK}
import cats.data.Validated
import cats.data.NonEmptyList

class ValidatedEx extends FlatSpecLike with Matchers {

  def parallelValidate[E: Semigroup, A, B, C](v1: Validated[E, A], v2: Validated[E, B])(f: (A, B) => C): Validated[E, C] =
    (v1, v2) match {
      case (Valid(a), Valid(b)) => Valid(f(a, b))
      case (Valid(_), i@Invalid(_)) => i
      case (i@Invalid(_), Valid(_)) => i
      case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
    }

  import cats.Apply
  import cats.data.ValidatedNel

  implicit val nelSemigroup: Semigroup[NonEmptyList[ConfigError]] =
    SemigroupK[NonEmptyList].algebra[ConfigError]


  case class Address(houseNumber: Int, street: String)

  case class Person(name: String, age: Int, address: Address)

  "valid data" should "be wrapped in a valid instance" in {
    val config = Config(Map(("url", "123.0.0.1"), ("port", "1337")))

    val valid = parallelValidate(
      config.parse[String]("url").toValidatedNel,
      config.parse[Int]("port").toValidatedNel)(ConnectionParams.apply)

    valid.isValid shouldBe true
    valid.getOrElse(ConnectionParams("", 0)) shouldBe ConnectionParams("123.0.0.1", 1337)
  }

  "multiple errors" should "be accumulated in a NonEmptyList wrapped in an Invalid instance" in {
    val config = Config(Map(("endpoint", "127.0.0.1"), ("port", "not a number")))

    val invalid = parallelValidate(
      config.parse[String]("url").toValidatedNel,
      config.parse[Int]("port").toValidatedNel
    )(ConnectionParams.apply)

    invalid.isValid shouldBe false
    val errors = NonEmptyList(MissingConfig("url"), ParseError("port") :: Nil)
    invalid shouldBe Validated.invalid(errors)
  }


  "sequential validation" should "be an option" in {
    val config = Config(Map(("house_number", "-42")))
    val houseNumber = config.parse[Int]("house_number").andThen { n =>
      if (n >= 0) Validated.valid(n)
      else Validated.invalid(ParseError("house_number"))
    }

    houseNumber.isValid shouldBe false
    val error = ParseError("house_number")
    houseNumber == Validated.invalid(error) shouldBe true
  }

  "using Validated" should "allow to use short-circuiting behaviour" in {

    import cats.data.Xor

    def positive(field: String, i: Int): ConfigError Xor Int = {
      if (i >= 0) Xor.right(1)
      else Xor.left(ParseError(field))
    }
    val config = Config(Map("house_number" -> "-42"))
    val houseNumber = config.parse[Int]("house_number").withXor { (xor: ConfigError Xor Int) =>
      xor.flatMap { i =>
        positive("house_number", i)
      }
    }

    houseNumber.isValid shouldBe false
    val error = ParseError("house_number")
    houseNumber == Validated.invalid(error) shouldBe true
  }

  import cats.Applicative

  implicit def validatedApplicative[E: Semigroup]: Applicative[({type A[B] = Validated[E, B]})#A] =
    new Applicative[({type A[B] = Validated[E, B]})#A] {
      def ap[A, B](f: Validated[E, A => B])(fa: Validated[E, A]): Validated[E, B] =
        (fa, f) match {
          case (Valid(a), Valid(fab)) => Valid(fab(a))
          case (i@Invalid(_), Valid(_)) => i
          case (Valid(_), i@Invalid(_)) => i
          case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
        }

      def pure[A](x: A): Validated[E, A] = Validated.valid(x)

      override def map[A, B](fa: Validated[E, A])(f: A => B): Validated[E, B] = fa.map(f)

      override def product[A, B](fa: Validated[E, A], fb: Validated[E, B]): Validated[E, (A, B)] =
        ap(fa.map(a => (b: B) => (a, b)))(fb)
    }
}

case class ConnectionParams(url: String, port: Int)

trait Read[A] {
  def read(s: String): Option[A]
}

object Read {
  def apply[A](implicit A: Read[A]): Read[A] = A

  implicit val stringRead: Read[String] =
    new Read[String] {
      override def read(s: String): Option[String] = Some(s)
    }

  implicit val intRead: Read[Int] =
    new Read[Int] {
      override def read(s: String): Option[Int] = if (s.matches("-?[0-9]+")) Some(s.toInt) else None
    }
}


sealed abstract class ConfigError

final case class MissingConfig(field: String) extends ConfigError

final case class ParseError(field: String) extends ConfigError

case class Config(map: Map[String, String]) {
  def parse[A: Read](key: String): Validated[ConfigError, A] =
    map.get(key) match {
      case None => Invalid(MissingConfig(key))
      case Some(value) =>
        Read[A].read(value) match {
          case None => Invalid(ParseError(key))
          case Some(a) => Valid(a)
        }
    }
}


