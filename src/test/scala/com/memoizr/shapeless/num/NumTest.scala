package com.memoizr.shapeless.num

import org.scalatest.FlatSpecLike
import shapeless._
import shapeless.ops.nat.Mod.Aux
import shapeless.ops.nat._
import shapeless.syntax.nat.*--*

class NumTest extends FlatSpecLike {

  it should "count" in {
    val three = Sum[Nat._3, Nat._3]
    println(Nat.toInt[Succ[three.Out#N]])
    println(Nat.toInt[Succ[Succ[_0]]])
  }

  object helper extends Poly1 {

    //    implicit def anInt[n <: Nat](implicit ev: ToInt[n]) = at[n] { x => ev.apply() }
    //    implicit def anInt[n <: Nat](implicit ev: ToInt[n]) = at[n] { x => ev.apply() }

//    type Aux[A <: Nat, B <: Nat, C <: Nat] = Sum[A, B] { type Out = C }
//
//    implicit def sum1[B <: Nat]: Aux[_0, B, B] = new Sum[_0, B] { type Out = B }
//    implicit def sum2[A <: Nat, B <: Nat]
//    (implicit sum : Sum[A, Succ[B]]): Aux[Succ[A], B, sum.Out] = new Sum[Succ[A], B] { type Out = sum.Out }

//    implicit def anInt[n <: Nat](implicit one: ToInt[n],
////                                 b: n,
//                                 aux : Mod[n, Nat._2]#Out#N =:= Nat._0
//                                ) = at[({type x <: Nat})#x] { x => one.apply().toString + "zero" }


//    implicit def anInts[n <: Nat](implicit ev: n =:= Nat._1,
//                                 one: ToInt[n],
//                                 zero: n =:= Nat._1
//                                ) = at[n] { x => one.apply().toString }

    class Multiple2[n]
//    implicit def foo[n <: Nat](implicit ev: Mod[n, Nat._2]#Out#N =:= Nat._0) = new Multiple2[n]
//    implicit def defun[n <: Nat](implicit ev: n) = at[n] { x => iseven[ev.N].toString; "hey"}

    //    implicit def default[n <: Nat] = at[n] { x => "zero" }

    implicit def defaults[n] = at[n] { x => "x" }
  }


  def iseven[n <: Nat](implicit mod : Mod[n, Nat._2]) = new Check[mod.Out] {}

  it should "create a range" in {
    type range = Range[_0, Succ[_0]]#Out
    type y = Sum[Nat._2, Nat._2]#Out
    println(the[Nat._0 *--* Nat._10].apply().map(helper))
    //    println(Nat.toInt[x.Out])
    //    val x = Nat.toInt[range.type]
    val x = iseven[Nat._8]
    println(check(0)(x))
  }

  trait Check[N <: Nat]
  def check(expected: Nat)(actually : => Check[expected.N]) {}

  implicit def empty[H, T <: HList](implicit ev: HList) = {
  }

}
