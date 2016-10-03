package com.memoizr.shapeless.typechecking

import org.scalatest.{FlatSpecLike, Matchers}
import shapeless.test.illTyped

import scala.util.Try

class TypeCheckingEx extends FlatSpecLike with Matchers {
  "illtyped" should "test for noncompilation" in {
    illTyped { """1+1: Boolean"""}
  }

  "assertTypeError" should "test for noncompilation as well" in {
    val matchedTypes = Try { assertTypeError("illTyped { \" val a: Int = 1\"}")}.isSuccess
    matchedTypes shouldBe true

    val mismatchedTypes = Try { assertTypeError("illTyped { \" val a: String = 1\"}")}.isSuccess
    mismatchedTypes shouldBe false
  }

}
