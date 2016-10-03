package com.memoizr.shapeless.typesafecasts

import org.scalatest.{FlatSpecLike, Matchers}

class TypeSafeCastsEx extends FlatSpecLike with Matchers {

  import shapeless._
  import syntax.typeable._

  "Typeable" should "provide safe casts" in {
    val l: Any = List(Vector("foo", "var", "baz"), Vector("wibble"))
    l.cast[List[Vector[String]]] shouldBe Some(List(Vector("foo", "var", "baz"), Vector("wibble")))
    l.cast[List[Vector[Int]]] shouldBe None
    l.cast[List[List[String]]] shouldBe None
  }

  it should "provide more precision in pattern matching" in {
    val `List[String]` = TypeCase[List[String]]
    val `List[Int]` = TypeCase[List[Int]]
    val l = List(1, 2, 3)

    val result = (l: Any) match {
      case `List[String]` (List(s, _*)) => s.length
      case `List[Int]` (List(i, _*)) => i + 1
    }

    result shouldBe 2
  }


}
