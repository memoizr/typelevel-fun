package com.memoizr.cats.xor

import org.scalatest.{FlatSpecLike, Matchers}

class XorEx extends FlatSpecLike with Matchers {

  "good old Either" should "returns another Either after a transformation" in {
    val e1: Either[String, Int] = Right(5)
    val x = e1.right.map(_ + 1)
    x shouldBe Right(6)

    val e2: Either[String, Int] = Left("Hello")
    val y = e2.left.map(_ + 1)

    y shouldBe Left("Hello1")
  }

  import cats.data.Xor
  import cats.data.Xor.Right
  import cats.data.Xor.Left

  "Xor" should "be right biased" in {

    val right: String Xor Int = Xor right 5
    right.map(_ + 1) shouldBe Right(6)

    val left: String Xor Int = Xor left "something went wrong"
    left map (_ + 1) shouldBe Left("something went wrong")
  }

  import cats.Monad

  implicit def xorMonad[Err]: Monad[({type A[B] = Xor[Err, B]})#A] = new Monad[({type A[B] = Xor[Err, B]})#A] {
    override def pure[A](x: A): Xor[Err, A] = Xor right x

    override def flatMap[A, B](fa: Xor[Err, A])(f: (A) => Xor[Err, B]): Xor[Err, B] = fa flatMap f

    override def tailRecM[A, B](a: A)(f: (A) => Xor[Err, Either[A, B]]): Xor[Err, B] = ???
  }

  "Xor flatMap" should "be right biased" in {
    val right: String Xor Int = Xor right 5
    right.flatMap(x => Xor.right(x + 1)) shouldBe Right(6)

    val left: String Xor Int = Xor left "something went wrong"
    left.flatMap(x => Xor.right(x + 1)) shouldBe Left("something went wrong")
  }

  object XorStyle {
    def parse(s: String): Xor[NumberFormatException, Int] =
      if (s matches "-?[0-9]+") Xor right s.toInt
      else Xor left new NumberFormatException(s"${s} is not a valid integer.")

    def reciprocal(i: Int): Xor[IllegalArgumentException, Double] =
      if (i == 0) Xor.left(new IllegalArgumentException("Cannot take reciprocal of 0."))
      else Xor.right(1.0 / i)

    def stringify(d: Double): String = d.toString

    def magic(s: String): Xor[Exception, String] =
      parse(s).flatMap(reciprocal).map(stringify)
  }

  "Xor" should "wrap exceptions" in {
    XorStyle.parse("Not a number").isRight shouldBe false
    XorStyle.parse("2").isRight shouldBe true

    XorStyle.magic("Not a number").isRight shouldBe false
    XorStyle.magic("0").isRight shouldBe false
    XorStyle.magic("1").isRight shouldBe true
  }

  it should "allow for exhaustive pattern matching" in {
    import XorStyle._
    val result = magic("2") match {
      case Left(_: NumberFormatException) => "NaN"
      case Left(_: IllegalArgumentException) => "Division by 0"
      case Left(_) => "unknown"
      case Right(res) => s"reciprocal is $res"
    }

    result shouldBe "reciprocal is 0.5"
  }

  object XorStyleWithAdts {

    sealed abstract class Error

    final case class NotANumber(string: String) extends Error

    case object NoZeroReciprocal extends Error

    def parse(s: String): Xor[Error, Int] =
      if (s.matches("-?[0-9]+")) Xor.right(s.toInt)
      else Xor.left(NotANumber(s))

    def reciprocal(i: Int): Xor[Error, Double] =
      if (i == 0) Xor.left(NoZeroReciprocal)
      else Xor.right(1.0 / i)

    def stringify(d: Double): String = d.toString

    def magic(s: String): Xor[Error, String] =
      parse(s).flatMap(reciprocal).map(stringify)
  }


  "using ADTs instead of exceptions" should "result in a fully exhaustive pattern match" in {
    import XorStyleWithAdts._
    val result = magic("2") match {
      case Left(NotANumber(_)) => "NaN"
      case Left(NoZeroReciprocal) => "Division by 0"
      case Right(res) => s"reciprocal is $res"
    }

    result shouldBe "reciprocal is 0.5"
  }

  "leftMap" should "apply function to the left side" in {
    val right: String Xor Int = Xor.Right(41)
    right.map(_ + 1) shouldBe Right(42)

    val left: String Xor Int = Xor.Left("Hello")
    left.map(_ + 1) shouldBe Xor.Left("Hello")
    left.leftMap(_.reverse) shouldBe Left("olleH")
  }

  "catchOnly and catchNonFatal" should "offer a bridge between exception-throwing code and functional code" in {
    Xor.catchOnly[NumberFormatException]("abc".toInt).isRight shouldBe false
    Xor.catchNonFatal(1/0).isLeft shouldBe true
  }

  "additional syntax" should "make left and right methods available on objects" in {
    import cats.syntax.xor._

    val right: String Xor Int = 42.right[String]
    right shouldBe Right(42)
  }
}
