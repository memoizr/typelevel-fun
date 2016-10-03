package com.memoizr.shapeless.autotypeclassderivation

import org.scalatest.{FlatSpecLike, Matchers}
import shapeless.{HList, HNil, ProductTypeClass, ProductTypeClassCompanion}

class AutoTypeclassDerivationEx extends FlatSpecLike with Matchers {

  case class Foo(i: Int, s: String)

  case class Bar(b: Boolean, s: String, d: Double)

  import MonoidSyntax._
  import Monoid.typeClass._

  "auto typeclasses" should "be automatically derivable for monoids" in {
    val fooCombined = Foo(13, "foo") |+| Foo(23, "bar")
    fooCombined shouldBe Foo(36, "foobar")

    val barCombined = Bar(true, "foo", 1.0) |+| Bar(false, "bar", 3.0)
    barCombined shouldBe Bar(true, "foobar", 4.0)
  }

}

trait SemiGroup[T] {
  def append(a: T, b: T): T
}

trait Monoid[T] extends SemiGroup[T] {
  def zero: T

  override def append(a: T, b: T): T
}

object Monoid extends ProductTypeClassCompanion[Monoid] {

  import shapeless._

  def mzero[T](implicit mt: Monoid[T]) = mt.zero

  def createMonoid[T](mzero: T)(mappend: (T, T) => T) = new Monoid[T] {
    override def zero = mzero

    override def append(a: T, b: T) = mappend(a, b)
  }

  implicit def booleanMonoid: Monoid[Boolean] = createMonoid(false) { (a, b) => a || b }

  implicit def intMonoid: Monoid[Int] = createMonoid(0) {
    _ + _
  }

  implicit def doubleMonoid: Monoid[Double] = createMonoid(0.0) {
    _ + _
  }

  implicit def stringMonoid: Monoid[String] = createMonoid("") {
    _ + _
  }

  object typeClass extends ProductTypeClass[Monoid] {
    override def emptyProduct: Monoid[HNil] = createMonoid[HNil](HNil) { (_, _) => HNil }

    override def product[F, T <: HList](mh: Monoid[F], mt: Monoid[T]) = createMonoid[F :: T](mh.zero :: mt.zero) { (a, b) =>
      mh.append(a.head, b.head) :: mt.append(a.tail, b.tail)
    }

    override def project[F, G](instance: => Monoid[G], to: F => G, from: G => F) = createMonoid(from(instance.zero)) { (a, b) =>
      from(instance.append(to(a), to(b)))
    }
  }
}

trait MonoidSyntax[T] {
  def |+|(b: T): T
}

object MonoidSyntax {
  implicit def monoidSyntax[T](a: T)(implicit mt: Monoid[T]): MonoidSyntax[T] = new MonoidSyntax[T] {
    def |+|(b: T) = mt.append(a, b)
  }
}
