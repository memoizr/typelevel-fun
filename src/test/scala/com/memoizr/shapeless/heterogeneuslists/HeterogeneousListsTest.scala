package com.memoizr.shapeless.heterogeneuslists

import com.memoizr.shapeless.Size
import org.scalatest.{FlatSpec, Matchers}
import shapeless.syntax.zipper.toZipper
import shapeless.{HNil, Poly2}

class HeterogeneousListsTest extends FlatSpec with Matchers {

  import shapeless.poly._

  object choose extends (Set ~> Option) {
    def apply[T](s: Set[T]) = s.headOption
  }

  "a heterogeneous list" should "concat values, map them using a poly" in {
    val sets = Set(1) :: Set("foo") :: HNil

    val opts = sets map choose

    opts should be(Option(1) :: Option("foo") :: HNil)
  }

  it should "also have a flatmap operation" in {
    val list = (23 :: "foo" :: HNil) :: HNil :: (true :: HNil) :: HNil

    list flatMap identity should be(23 :: "foo" :: true :: HNil)
  }

  object addSize extends Poly2 {
    implicit def default[T](implicit st: Size.Case.Aux[T, Int]) = at[Int, T] { (acc, t) => acc + Size(t) }

  }

  it should "have a fold which is sensitive to types" in {
    val list = 23 :: "foo" :: (13, "wibble") :: HNil
    list.foldLeft(0)(addSize) should be(11)
  }

  it should "have a zipper for traversal and persistent update" in {
    val l = 1 :: "foo" :: 3.0 :: HNil
    l.toZipper.right.put(("wibble", 45)).reify should be(1 :: ("wibble", 45) :: 3.0 :: HNil)
    l.toZipper.right.delete.reify should be(1 :: 3.0 :: HNil)
  }

  it should "be covariant" in {
    CovariantHelper.x should be(true)
    CovariantHelper.apap.isInstanceOf[FFFF] shouldBe true
    CovariantHelper.apap.unify.isInstanceOf[FFFF] shouldBe true
  }

  it should "support conversion to list using least upper bound" in {
    CovariantHelper.apap.toList should be(List(Apple(), Pear(), Apple(), Pear()))
  }

  it should "support typeable lists" in {
    import shapeless.syntax.typeable._

    val ffff: FFFF = CovariantHelper.apap.unify
    val precise: Option[APAP] = ffff.cast[APAP]

    precise should be(Some(Apple() :: Pear() :: Apple() :: Pear() :: HNil))


    import shapeless.syntax.std.tuple._
    (23, "foo", true).drop(2) should be(Tuple1(_1 = true))
  }

  trait Fruit

  case class Apple() extends Fruit

  case class Pear() extends Fruit

  import shapeless.::

  type FFFF = Fruit :: Fruit :: Fruit :: Fruit :: HNil
  type APAP = Apple :: Pear :: Apple :: Pear :: HNil

  object CovariantHelper {


    val a: Apple = Apple()
    val p: Pear = Pear()

    val apap: APAP = a :: p :: a :: p :: HNil

    import scala.reflect.runtime.universe._

    val x = implicitly[TypeTag[APAP]].tpe.typeConstructor <:< typeOf[FFFF]
  }

}
