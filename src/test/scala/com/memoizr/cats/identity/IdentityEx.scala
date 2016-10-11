package com.memoizr.cats.identity

import org.scalatest.{FlatSpecLike, Matchers}

class IdentityEx extends FlatSpecLike with Matchers {
  //  type Id[A] = A
  import cats._

  val x: Id[Int] = 1
  val y: Int = x


  "Id[T] values" should "be directly comparable to unadorned instances of T" in {
    val anId: Id[Int] = 42
    anId shouldBe 42
  }

  "identity" should "behave like an applicative" in {
    val one: Int = 1
    Applicative[Id].pure(42) shouldBe 42
  }

  it should "have a Comonad instance" in {
    val fortytwo: Int = 42
    Comonad[Id].coflatMap(fortytwo)(_ + 1) shouldBe 43
  }
}
