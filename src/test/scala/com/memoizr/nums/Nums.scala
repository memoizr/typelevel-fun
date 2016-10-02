package com.memoizr.nums

import org.scalatest.FlatSpec
import com.memoizr.learning_scalaz.{Bool, False, True}


class NumTest extends FlatSpec {
  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type Is0[A <: Nat] = A#IsZero[False, True, Bool]
  type IsTrue[A <: Nat] = False#If[True, True, Bool]

  it should "be num " in {
    println(Bool.toBoolean[Succ[_0]#Compare[_0]#gt])

    println(toInt[Succ[Succ[_3]]])

    println(implicitly[_0 :: _1 :: _2 :: cnil].toList)
  }

  def toInt[T <: Nat : IntRep]: Int = {
    implicitly[IntRep[T]].int
  }

  class IntRep[+X <: Nat](val int: Int) {
  }

  type y = _0 :: _1 :: cnil
  implicit val zeroRep: IntRep[_0] = new IntRep(0)

  implicit def nonZeroRep[i <: Nat](implicit ev: IntRep[i#Pred]): IntRep[i] = new IntRep(1 + ev.int)

  implicit val nil: cnil = cnil
  //  implicit val znil: _0 :: cnil = null
  //  implicit val nils: clist = cnil
  implicit val bah: _0 = new _0() {}
  implicit val bahs: _1 = new _1() {}
  implicit val bahsss: _2 = new _2() {}

  implicit def rest[A, rest <: clist](implicit a: A, ev: rest): ::[A, rest] = cons(a, ev)
}

trait clist {
  type tail <: clist
  def toList: List[Any]
}

object clist {
  def ::(x: clist) = cnil
}

trait cnil extends clist {
  override type tail = cnil

  override def toList: List[Any] = Nil
}

object cnil extends cnil

trait ::[+A, b <: clist] extends clist {
  override type tail = b
}

case class cons[A, b <: clist](a: A, b: b) extends ::[A, b] {
  def toList: List[Any] = a :: b.toList
}

sealed trait Nat {
  type Bar <: Bool
  type Foo <: Bool
  type Pred <: Nat
  type IsZero[NonZero <: Up, IfZero <: Up, Up] <: Up
  type Compare[N <: Nat] <: Comparison
}

sealed trait _0 extends Nat {
  override type Bar = True
  override type Foo = True
  override type Pred = _0
  override type IsZero[NonZero <: Up, IfZero <: Up, Up] = IfZero
  override type Compare[N <: Nat] = N#IsZero[LT, EQ, Comparison]
}

sealed trait NonZero

sealed trait Succ[N <: Nat] extends Nat with NonZero {
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
