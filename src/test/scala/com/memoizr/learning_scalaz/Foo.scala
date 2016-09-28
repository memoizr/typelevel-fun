package com.memoizr.learning_scalaz

import com.memoizr.learning_scalaz.Bool._
import com.memoizr.learning_scalaz.nums.Nat
import org.scalatest.FlatSpec

import scalaz._

class Foo extends FlatSpec {
  it should "dosomething" in {
    println(toBoolean[True || False])
  }
}

trait Recurse {
  println("recur")
  type Next <: Recurse
  type X[R <: Recurse] <: Int
}

trait RecurseA extends Recurse {
  override type Next = RecurseA
  override type X[R <: Recurse] = R#X[R#Next]
}

sealed trait Bool {
  type If[T <: Up, F <: Up, Up] <: Up
  type AND[B <: Bool] <: Bool
}

object Bool {
  type &&[A <: Bool, B <: Bool] = A#If[B, False, Bool]
  type ||[A <: Bool, B <: Bool] = A#If[True, B, Bool]
  type Not[A <: Bool] = A#If[False, True, Bool]

  class BoolRep[B <: Bool](val value: Boolean)

  implicit val falseRep: BoolRep[False] = new BoolRep(false)
  implicit val trueRep: BoolRep[True] = new BoolRep(true)
  implicit val maybeRep: BoolRep[Nat#Bar] = new BoolRep(true)

  def toBoolean[B <: Bool](implicit boolean: BoolRep[B]): Boolean = boolean.value
}

trait True extends Bool {
  type If[T <: Up, F <: Up, Up] = T
  override type AND[B <: Bool] = B
}

trait False extends Bool {
  type If[T <: Up, F <: Up, Up] = F
  override type AND[B <: Bool] = False
}

