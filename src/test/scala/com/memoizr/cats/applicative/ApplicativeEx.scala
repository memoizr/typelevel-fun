package com.memoizr.cats.applicative

import org.scalatest.{FlatSpecLike, Matchers}

class ApplicativeEx extends FlatSpecLike with Matchers {

  import cats._
  import cats.implicits._


  "applicative pure" should "put the value into a context" in {
    Applicative[Option].pure(1) shouldBe Some(1)
    Applicative[List].pure(1) shouldBe List(1)
  }

   "applicative functors" should "be composable" in {
     (Applicative[List] compose Applicative[Option]).pure(1) shouldBe List(Some(1))
   }

  "applicative" should "be preferred to Monad when the structure of the computation is known a priori" in {
    Monad[Option].pure(1) shouldBe Some(1)
    Applicative[Option].pure(1) shouldBe Some(1)
  }

}
