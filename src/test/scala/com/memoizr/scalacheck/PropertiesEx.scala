package com.memoizr.scalacheck

import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.exceptions.GeneratorDrivenPropertyCheckFailedException

class PropertiesEx extends FlatSpec {
  import org.scalacheck.Prop._
  import org.scalatest.prop.Checkers._

  it should "have a forall mehtod" in {

    check {
      forAll { (l1: List[Int], l2: List[Int]) =>
        l1.size + l2.size == (l1 ::: l2).size
      }
    }

  }

  it should "fail sometimes" in {
    val propSquareRoot = forAll { (n: Int) => scala.math.sqrt(n * n) == n }
    intercept[GeneratorDrivenPropertyCheckFailedException] {
      check(propSquareRoot)
    }
  }

  it should "test constant properties consistently" in {
    check {
      forAll { (s1: String, s2: String) =>
        (s1 + s2).endsWith(s2)
      }
    }
  }

  val smallInteger = Gen.choose(0, 100)
  it should "use custom generators" in {
    check {
      forAll(smallInteger) { n =>
        n >= 0 && n <= 100
      }
    }
  }

  it should "look for implications" in {
    intercept[GeneratorDrivenPropertyCheckFailedException] {
      check {
        forAll { n: Int =>
          (n == 0) ==> (n + 10 == 10)
        }
      }
    }
  }

  it should "filter only for relevant implications" in {
    check {
      forAll { n: Int =>
        (n % 2 == 0) ==> (n % 2 == 0)
      }
    }
  }

  it should "allow to combine multiple properties" in {
    check {
      forAll(smallInteger) { n => !(n > 100) } &&
        forAll(smallInteger) { n => n >= 0 }
    }
  }

  it should "work for grouped properties" in {

    check(new ZeroSpecification())
  }

  import org.scalacheck.Properties

  class ZeroSpecification extends Properties("Zero") {
    import org.scalacheck.Prop.forAll

    property("addition property") = forAll { n: Int => (n != 0) ==> ((n + 0) == n)}
    property("additive inverse property") = forAll { n: Int => (n != 0) ==> (n + (-n) == 0)}
    property("multiplicative property") = forAll { n: Int => (n != 0) ==> ((n* 1) == n)}
  }
}
