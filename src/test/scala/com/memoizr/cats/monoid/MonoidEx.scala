package com.memoizr.cats.monoid

import org.scalatest.{FlatSpecLike, Matchers}

class MonoidEx extends FlatSpecLike with Matchers {

  import cats.implicits._
  import cats._

  "monoid" should "be a semigroup with a zero value" in {
    Monoid[String].empty shouldBe ""
    Monoid[String].combineAll(List("a", "b", "c")) shouldBe "abc"
    Monoid[String].combineAll(List()) shouldBe ""
  }

  it should "be composable with monoids of different types" in {
    Monoid[Map[String, Int]].combineAll(List(Map("a" -> 1, "b" -> 2), Map("a" -> 3))) shouldBe Map("a" -> 4, "b" -> 2)
    Monoid[Map[String, Int]].combineAll(List()) shouldBe Map()
  }

  it should "work with own types and foldMap" in {
    val l = List(1,2,3,4,5)
    l.foldMap(identity) shouldBe 15
    l.foldMap(i => i.toString) shouldBe "12345"
  }

  it should "work with tuples that have monoid representations inside" in {

    val l = List(1,2,3,4,5)
    l.foldMap(i => (i, i.toString)) shouldBe (15, "12345")
  }
}
