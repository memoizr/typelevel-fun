package com.memoizr.shapeless

import org.scalatest.{FlatSpec, Matchers}
import shapeless.Poly1
import shapeless.PolyDefns.~>

class ShapelessTest extends FlatSpec with Matchers {

  import shapeless.poly._

  "base transformation" should "transform type" in {

    choose(Set(1, 2, 3)) should be(Some(1))
    choose(Set('a', 'b', 'c')) should be(Some('a'))
  }

  def pairApply(transformation: Set ~> Option) = (transformation(Set(1, 2, 3)), transformation(Set('a', 'b', 'c')))

  it should "be able to be passed as a parameter to a function" in {
    pairApply(choose) should be(Some(1), Some('a'))
  }

  it should "be convertible to monomorphic function" in {
    List(Set(1, 3, 5), Set(2, 4, 6)).map(choose) should be(List(Some(1), Some(2)))
  }

  "a polymorphic functtion" should "be able to capture type-specific cases" in {
    import shapeless.poly._
//    Size(23) should be(1)
//    Size("foo") should be(3)
  }
}

object choose extends (Set ~> Option) {
  override def apply[T](s: Set[T]) = s.headOption
}

object Size extends Poly1 {
  implicit def caseInt = at[Int](x => 1)

  implicit def caseString = at[String](_.length())

  implicit def caseTuple[T, U](implicit st: Case.Aux[T, Int], su: Case.Aux[U, Int]) = at[(T, U)](t => Size(t._1) + Size(t._2))
}
