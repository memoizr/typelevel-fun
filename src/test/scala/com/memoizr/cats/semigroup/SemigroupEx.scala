package com.memoizr.cats.semigroup

import org.scalatest.{FlatSpecLike, Matchers}

class SemigroupEx extends FlatSpecLike with Matchers {

  import cats.implicits._
  import cats.kernel._

  "Semigroup" should "show the associative property" in {
    Semigroup[Int].combine(1, 3) shouldBe 4

    Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)

    Semigroup[Option[Int]].combine(Option(1), Option(2)) shouldBe Some(3)

    Semigroup[Option[Int]].combine(Option(1), None) shouldBe Some(1)

    Semigroup[Int => Int].combine({ (x: Int) => x + 1 }, { (x: Int) => x * 10 }).apply(6) shouldBe 67
  }

  it should "merge the maps value with combine" in {
    import cats.implicits._

    val aMap = Map("foo" -> Map("bar" -> 5))
    val anotherMap = Map("foo" -> Map("bar" -> 6))
    val combinedMap = Semigroup[Map[String, Map[String, Int]]].combine(aMap, anotherMap)

    combinedMap.get("foo") shouldBe Some(Map("bar" -> 11))
  }

  it should "support the hieroglyphic syntax" in {
    import cats.implicits._

    val one: Option[Int] = Option(1)
    val two: Option[Int] = Option(2)
    val n: Option[Int] = None

    one |+| two shouldBe Some(3)

    n |+| two shouldBe Some(2)
    two |+| n shouldBe Some(2)
    n |+| n shouldBe None
  }

}
