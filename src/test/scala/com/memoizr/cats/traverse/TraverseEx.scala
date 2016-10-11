package com.memoizr.cats.traverse

import org.scalatest.{FlatSpecLike, Matchers}

class TraverseEx extends FlatSpecLike with Matchers {

  import cats.data.Xor
  import scala.concurrent.Future

  import cats.Semigroup
  import cats.syntax.traverse._
  import cats.syntax.all._
  import cats.implicits._
  import cats.Traverse.ops._
  import cats.data.{NonEmptyList, OneAnd, Validated, ValidatedNel, Xor}

  def parseIntXor(s: String): Xor[NumberFormatException, Int] = Xor.catchOnly[NumberFormatException](s.toInt)

  def parseIntValidated(s: String): ValidatedNel[NumberFormatException, Int] = Validated.catchOnly[NumberFormatException](s.toInt).toValidatedNel

  "traverse" should "traverse structure and accumulate failures" in {
    List("1", "2", "3").traverseU(parseIntXor) shouldBe Xor.Right(List(1, 2, 3))
    List("1", "abc", "3").traverseU(parseIntXor).isLeft shouldBe  true
  }

  it should "traverse and do the validation stuff" in {
    List("1", "2", "3").traverseU(parseIntValidated).isValid shouldBe true
  }

  it should "be traversable with the identity function" in {
    List(Option(1), Option(2), Option(3)).traverse(identity) shouldBe Some(List(1, 2, 3))
    List(Option(1), None, Option(3)).traverse(identity) shouldBe None
  }

  "sequence_" should "ignore the result of each computation" in {
    List(Option(1), Option(2), Option(3)).sequence_ shouldBe Some()
    List(Option(1), None, Option(3)).sequence_ shouldBe None
  }
}
