package com.memoizr.fizzbuzz

import org.scalatest.FlatSpecLike

class FizzBuzzTest extends FlatSpecLike {

  import shapeless._
  import nat._0
  import ops.nat._
  import ops.hlist._
  import Nat._

  sealed trait FizzBuzz

  object FizzBuzz {

    case object Fizz extends FizzBuzz

    case object Buzz extends FizzBuzz

    case object FizzAndBuzz extends FizzBuzz

    final class Other[N <: Nat] extends FizzBuzz

  }

  import FizzBuzz._

  sealed trait NatToFizzBuzz[N <: Nat] extends DepFn0 {
    type Out <: FizzBuzz
  }

  sealed trait NatToFizzBuzzInstances1 {
    type Aux[N <: Nat, FB <: FizzBuzz] = NatToFizzBuzz[N] {type Out = FB}

    implicit def other[N <: Nat]: Aux[N, Other[N]] = new NatToFizzBuzz[N] {
      type Out = Other[N]

      def apply = new Other[N]
    }
  }

  sealed trait NatToFizzBuzzInstances0 extends NatToFizzBuzzInstances1 {
    implicit def fizz[N <: Nat](implicit ev: Mod.Aux[N, Nat._3, _0]): Aux[N, Fizz.type] =
      new NatToFizzBuzz[N] {
        type Out = Fizz.type

        def apply = Fizz
      }

    implicit def buzz[N <: Nat](implicit ev: Mod.Aux[N, Nat._5, _0]): Aux[N, Buzz.type] =
      new NatToFizzBuzz[N] {
        type Out = Buzz.type

        def apply = Buzz
      }
  }

  object NatToFizzBuzz extends NatToFizzBuzzInstances0 {
    implicit def fizzAndBuzz[N <: Nat](implicit fizz: Aux[N, Fizz.type], buzz: Aux[N, Buzz.type]): Aux[N, FizzAndBuzz.type] =
      new NatToFizzBuzz[N] {
        type Out = FizzAndBuzz.type

        def apply = FizzAndBuzz
      }
  }

  sealed trait RevFizzBuzz[N <: Nat] extends DepFn0 {
    type Out <: HList
  }

  object RevFizzBuzz {
    type Aux[N <: Nat, L <: HList] = RevFizzBuzz[N] {type Out = L}

    implicit def revFizzBuzzOne: Aux[_1, Other[_1] :: HNil] =
      new RevFizzBuzz[_1] {
        type Out = Other[_1] :: HNil

        def apply = new Other[_1] :: HNil
      }

    implicit def succRevFizzBuzz[N <: Nat](implicit f: RevFizzBuzz[N], n: NatToFizzBuzz[Succ[N]]): Aux[Succ[N], n.Out :: f.Out] =
      new RevFizzBuzz[Succ[N]] {
        type Out = n.Out :: f.Out

        def apply = n.apply :: f.apply
      }
  }

  sealed trait FizzBuzzResult[N <: Nat] extends DepFn0 {
    type Out <: HList
  }

  object FizzBuzzResult {
    type Aux[N <: Nat, L <: HList] = FizzBuzzResult[N] {type Out = L}

    implicit def fizzBuzzResult[N <: Nat, L <: HList](implicit rfb: RevFizzBuzz.Aux[N, L], r: Reverse[L]): Aux[N, r.Out] =
      new FizzBuzzResult[N] {
        type Out = r.Out

        def apply() = r(rfb())
      }
  }

  object FizzBuzzToString extends Poly1 {
    implicit val fizz = at[Fizz.type](_ => "fizz")
    implicit val buzz = at[Buzz.type](_ => "buzz")
    implicit val fizzAndBuzz = at[FizzAndBuzz.type](_ => "fizzbuzz")

    implicit def other[N <: Nat](implicit t: ToInt[N]) = at[Other[N]](n => t().toString)
  }

  def fizzBuzz[N <: Nat](implicit f: FizzBuzzResult[N]): f.Out = f()


  it should "print the fizzbuzz series" in {
    val r = fizzBuzz[_5]
    val s = r.map(FizzBuzzToString).mkString("", "\n", "")
    print(s)
  }
}
