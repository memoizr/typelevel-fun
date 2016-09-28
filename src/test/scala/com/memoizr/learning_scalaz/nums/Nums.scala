package com.memoizr.learning_scalaz.nums

import org.scalatest.FlatSpec
import com.memoizr.learning_scalaz.{Bool, False, True}


class NumTest extends FlatSpec {
  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type Is0[A <: Nat] = A#IsZero[False, True, Bool]
  type IsTrue[A <: Nat] = False#If[True, True, Bool]
  it should "be num " in {
    //    println(toBoolean[Is0[_2]])
    //    println(toBoolean[_0#Compare[_1]#lt])
    //    println(toBoolean[_1#Compare[_1]#lt])
    println(Bool.toBoolean[Succ[_0]#Compare[_0]#gt])
//    toI[_0]
        println(toInt[_0])
//    println(toInt[_1])
  }

  class IntRep[+X <: Nat](val int: Int) {
  }

  implicit val zeroRep: IntRep[_0] = new IntRep(0)
//  implicit val zeroRep: IntRep[Nat#Pred] = new IntRep(0)
  implicit val natRep: IntRep[Nat] = new IntRep(1)

//  def toInt[N : Nat](implicit intRep: IntRep[N]): Int = {
////    if (intRep.int == 0) 0 else 1 + toInt[N#Pred]
//    implicitly[N]
//    0
//  }
  def toInt[N <: Nat]: Int = {
    //    if (intRep.int == 0) 0 else 1 + toInt[N#Pred]
//    implicitly[N]
    0
  }
}

trait xx extends Nat {
}

sealed trait Nat {
  type Bar <: Bool
  type Foo <: Bool
  type Cheat <: xx
  type Pred <: Nat
  type IsZero[NonZero <: Up, IfZero <: Up, Up] <: Up
  type Compare[N <: Nat] <: Comparison
}

object Nat {
  def foo = 1
}

sealed trait _0 extends Nat {
  override type Bar = True
  override type Foo = True
  override type Pred = _0
  override type IsZero[NonZero <: Up, IfZero <: Up, Up] = IfZero
  override type Compare[N <: Nat] = N#IsZero[LT, EQ, Comparison]
}

sealed trait Succ[N <: Nat] extends Nat {
  override type Bar = False
  override type Foo = False
  override type Pred = N
  override type IsZero[NonZero <: Up, IfZero <: Up, Up] = NonZero
  override type Compare[O <: Nat] = O#IsZero[N#Compare[N], GT, Comparison]
}

trait NatOps {
  type _1 = Succ[_0]
  type Bah[A <: Nat] = A#IsZero[False, True, Bool]
}

sealed trait Comparison {
  type Match[IfLt <: Up, IfEq <: Up, IfGt <: Up, Up] <: Up
  type Bar[Up] <: Up
  type gt = Match[False, False, True, Bool]
  type ge = Match[False, True, True, Bool]
  type eq = Match[False, True, False, Bool]
  type le = Match[True, True, False, Bool]
  type lt = Match[True, False, False, Bool]
}

sealed trait GT extends Comparison {
  override type Match[IfLt <: Up, IfEq <: Up, IfGt <: Up, Up] = IfGt
}

sealed trait LT extends Comparison {
  override type Match[IfLt <: Up, IfEq <: Up, IfGt <: Up, Up] = IfLt
}

sealed trait EQ extends Comparison {
  override type Match[IfLt <: Up, IfEq <: Up, IfGt <: Up, Up] = IfEq
}
