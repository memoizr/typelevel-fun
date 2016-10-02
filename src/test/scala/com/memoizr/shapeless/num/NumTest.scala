package com.memoizr.shapeless.num

import org.scalatest.FlatSpecLike
import shapeless.{Nat, Poly1, the}
import shapeless.ops.nat.{Mod, ToInt}
import shapeless.syntax.nat.*--*

class NumTest extends FlatSpecLike {

  import shapeless.nat._

  trait FizzBuzz {
    def value: String
  }

  object Buzz extends FizzBuzz {
    override val value: String = "buzz"
  }

  object Fizz extends FizzBuzz {
    override val value: String = "fizz"
  }

  object FizzBuzz extends FizzBuzz {
    override val value: String = "fizzbuzz"
  }

  class OtherFizzBuzz(val int: Int) extends FizzBuzz {
    def value = int.toString
  }

  trait FizzBuzzConversion[N <: Nat] {
    type FizzBuzzType <: FizzBuzz

    def fizzBuzzType(): FizzBuzzType
  }

  sealed trait DefaultConversion {
    implicit def caseNeither[N <: Nat : ToInt] = new FizzBuzzConversion[N]() {
      override type FizzBuzzType = OtherFizzBuzz

      override def fizzBuzzType(): OtherFizzBuzz = new OtherFizzBuzz(implicitly[ToInt[N]].apply())
    }
  }

  type isDivisibleBy5[N <: Nat] = Mod.Aux[N, _5, _0]
  type isDivisibleBy3[N <: Nat] = Mod.Aux[N, _3, _0]

  sealed trait DivisibleBy3Or5Conversion extends DefaultConversion {
    implicit def caseFizz[N <: Nat : isDivisibleBy3] = new FizzBuzzConversion[N] {
      override type FizzBuzzType = Fizz.type

      override def fizzBuzzType(): Fizz.type = Fizz
    }

    implicit def caseBuzz[N <: Nat : isDivisibleBy5] = new FizzBuzzConversion[N] {
      override type FizzBuzzType = Buzz.type

      override def fizzBuzzType(): Buzz.type = Buzz
    }
  }

  sealed trait DivisibleByBoth3And5Conversion extends DivisibleBy3Or5Conversion {
    implicit def caseFizzBuzz[N <: Nat : isDivisibleBy5 : isDivisibleBy3] =
      new FizzBuzzConversion[N] {
        override type FizzBuzzType = FizzBuzz.type

        override def fizzBuzzType(): FizzBuzz.type = FizzBuzz
      }
  }

  object FizzBuzzConversion extends DivisibleByBoth3And5Conversion

  object fromNatToFizzBuzz extends Poly1 {
    implicit def convertToFizzBuzz[N <: Nat](implicit isThereA: FizzBuzzConversion[N]) = at[N] { _ => isThereA.fizzBuzzType().value }
  }

  it should "print fizzbuzz" in {
    the[_1 *--* _15].apply().map(fromNatToFizzBuzz).toList.foreach(println)
  }
}
