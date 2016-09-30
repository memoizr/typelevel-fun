package com.memoizr.shapeless.coproducts

import org.scalatest.{FlatSpecLike, Matchers}
import shapeless._

class CoproductsTest extends FlatSpecLike with Matchers {
  type ISB = Int :+: String :+: Boolean :+: CNil
  val isb = Coproduct[ISB]("foo")
  "coproduct" should "support section" in {
    isb.select[String] shouldBe Some("foo")
    isb.select[Int] shouldBe None
//    isb.select[Double] shouldBe None // Doesn't compile
  }

  object sizeM extends Poly1 {
    implicit def caseInt = at[Int] (i => (i, i))
    implicit def caseString = at [String] (s => (s, s.length))
    implicit def caseBoolean = at[Boolean] (b => (b,1))
  }

  it should "support mapping given a polymorphic function" in {
    val m = isb map sizeM
    m.select[(String, Int)] shouldBe Some("foo", 3)
  }

  import union._
  import syntax.singleton._

  type U = Union.`'i -> String, 's -> String, 'b -> Boolean`.T
  "adding labels to elements" should "return a discriminated union" in {
    val u = Coproduct[U]('s ->> "foo")

    u.get('i) shouldBe None
    u.get('s) shouldBe Some("foo")
    u.get('b) shouldBe None
  }
}
