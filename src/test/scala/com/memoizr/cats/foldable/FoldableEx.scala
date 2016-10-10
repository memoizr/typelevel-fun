package com.memoizr.cats.foldable

import org.scalatest.{FlatSpecLike, Matchers}

class FoldableEx extends FlatSpecLike with Matchers {

  import cats._

  "foldleft" should "perform a fold from left to right" in {
    import cats.implicits._
    Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6
    Foldable[List].foldLeft(List("a", "b", "c"), "")(_ + _) shouldBe "abc"
  }

  "foldright" should "be a lazy right associative fold on a foldable" in {
    import cats.implicits._
    val lazyResult = Foldable[List].foldRight(List(1, 2, 3), Now(0))((x, rest) => Later(x + rest.value))
    lazyResult.value shouldBe 6
  }

  import cats.implicits._

  "fold" should "combine all values in the foldable using the Monoid instance in scope" in {
    implicit val monoidInt: Monoid[Int] = new Monoid[Int] {
      override def empty: Int = 1

      override def combine(x: Int, y: Int): Int = x * y
    }
    Foldable[List].fold(List("a", "b", "c")) shouldBe "abc"

    Foldable[List].fold(List(1, 2, 3, 4))(monoidInt) shouldBe 24
  }

  "foldMap" should "map from A to B, then fold using the given Monoid instance" in {
    Foldable[List].foldMap(List("a", "b", "c"))(_.length) shouldBe 3
    Foldable[List].foldMap(List(1, 2, 3))(_.toString) shouldBe "123"
  }

  "foldK" should "combine using MonoidK[G]" in {
    Foldable[List].foldK(List(List(1, 2), List(3, 4, 5))) shouldBe List(1, 2, 3, 4, 5)
    Foldable[List].foldK(List(Option("three"), None, Option("two"))) shouldBe Some("three")
  }

  "find" should "search for the first element that matches a predicate, if it exists" in {
    Foldable[List].find(List(1, 2, 3))(_ > 2) shouldBe Some(3)
    Foldable[List].find(List(1, 2, 3))(_ > 5) shouldBe None
  }

  "exists" should "check an element exists that satisfies the specified criteria" in {
    Foldable[List].exists(List(1, 2, 3, 4))(_ == 3) shouldBe true
    Foldable[List].exists(List(1, 2, 3, 4))(_ == 6) shouldBe false
  }

  "forall" should "check provided criteria is valid for all values" in {
    Foldable[List].forall(List(1, 2, 3, 4))(_ < 6) shouldBe true
    Foldable[List].forall(List(1, 2, 3, 4))(_ < 3) shouldBe false
  }

  "toList" should "convert foldable to list" in {
    Foldable[List].toList(List(1, 2, 3, 4)) shouldBe List(1, 2, 3, 4)
    Foldable[Option].toList(Some(3)) shouldBe List(3)
    Foldable[Option].toList(None) shouldBe List()
  }

  "filter_" should "filter elements that do not satisfy the condition" in {
    Foldable[List].filter_(List(1, 2, 3, 4, 5))(_ < 4) shouldBe List(1, 2, 3)
    Foldable[Option].filter_(Option(3))(_ < 2) shouldBe List()
  }

  "traverse" should "traverse a foldable" in {
    import cats.data.Xor
    def parseInt(s: String): Option[Int] = Xor.catchOnly[NumberFormatException](s.toInt).toOption

    Foldable[List].traverse_(List("1", "2", "3"))(parseInt) shouldBe Some()
    Foldable[List].traverse_(List("1", "b", "3"))(parseInt) shouldBe None
  }

  "compose" should "compose Foldable[F[_]] with Foldable[G[_]]" in {
    val FoldableListOption = Foldable[List].compose[Option]
    FoldableListOption.fold(List(Option(1), Option(2), Option(3), Option(4))) shouldBe 10
    FoldableListOption.fold(List(Option("1"), Option("2"), Option("3"), Option("4"))) shouldBe "1234"
  }

  "other methods" should "also be a thing" in {
    Foldable[List].isEmpty(List(1, 2, 3)) shouldBe false
    Foldable[List].dropWhile_(List(1, 2, 3))(_ < 3) shouldBe List(3)
    Foldable[List].takeWhile_(List(1, 2, 3))(_ < 3) shouldBe List(1, 2)
  }
}
