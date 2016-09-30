package com.memoizr.shapeless.tuples

import org.scalatest.{FlatSpec, Matchers}
import shapeless.{HNil, Id, Poly1, Poly2}

class Tuples extends FlatSpec with Matchers {

  import shapeless.syntax.std.tuple._

  "tuples" should "give head" in {
    (23, "foo", true).head should be(23)
  }

  it should "get some tail" in {
    (23, "foo", true).tail should be("foo", true)
  }

  it should "drop as many as needed" in {
    (23, 4.0, "foo", true).drop(2) should be("foo", true)
  }

  it should "take as many as needed" in {
    (23, "foo", true).take(2) should be(23, "foo")
  }

  it should "split the splittles" in {
    (23, "foo", true).split(1) should be(Tuple1(23), ("foo", true))
  }

  it should "prepend like a champ" in {
    23 +: ("foo", true) should be((23, "foo", true))
  }

  it should "append" in {
    ("foo", true) :+ 23 should be(("foo", true, 23))
  }

  it should "concatenate" in {
    ("foo", true) ++ ("bar", 3) should be(("foo", true, "bar", 3))
  }

  import shapeless.poly._

  object option extends (Id ~> Option) {
    def apply[T](t: T) = Option(t)
  }

  it should "map" in {
    ("foo", true) map option should be(Some("foo"), Some(true))
  }

  it should "flatMap" in {
    ((23, "foo"), (), (true, 2.0)) flatMap identity should be((23, "foo", true, 2.0))
  }

  object Size extends Poly1 {
    implicit def caseInt = at[Int](x => 1)

    implicit def caseString = at[String](_.length)

    implicit def caseTuple[T, U](implicit st: Case.Aux[T, Int], su: Case.Aux[U, Int]) =
      at[(T, U)](t => Size(t._1) + Size(t._2))
  }

  object addSize extends Poly2 {
    implicit def default[T](implicit st: Size.Case.Aux[T, Int]) =
      at[Int, T] { (acc, t) => acc + Size(t) }
  }

  it should "fold" in {
    (23, "foo", (13, "wibble")).foldLeft(0)(addSize) should be(11)
  }

  it should "convert to HList" in {
    (23, "foo", true).productElements should be(23 :: "foo" :: true:: HNil)
  }

  it should "convert to list" in {
    (23, "foo", true).toList should be(List(23, "foo", true))
  }

  it should "zip" in {
    import shapeless.syntax.zipper._

    val l = (23, ("foo", true), 2.0).toZipper.right.down.put("bar").root.reify

    l should be((23, ("bar", true), 2.0))
  }
}
