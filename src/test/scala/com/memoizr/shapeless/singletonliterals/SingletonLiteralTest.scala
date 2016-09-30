package com.memoizr.shapeless.singletonliterals

import org.scalatest.{FlatSpecLike, Matchers}
import shapeless._

class SingletonLiteralTest extends FlatSpecLike with Matchers {
  import syntax.std.tuple._


  "lists and tuples" should "be representable as singleton literals" in {

    val hList = 23 :: "foo" :: true :: HNil
    hList(1) shouldBe "foo"
    val tuple = (23, "foo", true)
    tuple(2) shouldBe true
  }

  "all literals" should "have a value" in {
    23.leftSideValue shouldBe 23
    "foo".leftSideValue shouldBe "foo"
  }

  val (wTrue, wFalse) = (Witness(true), Witness(false))

  type True = wTrue.T
  type False = wFalse.T

  trait Select[B] { type Out }

  implicit val selInt = new Select[True] { override type Out = Int }
  implicit val selString = new Select[False] { override type Out = String }

  def select(b: WitnessWith[Select])(t: b.instance.Out) = t

  it should "find a conversion" in {
    select(true)(23) shouldBe 23
    select(false)("foo") shouldBe "foo"
  }

}
