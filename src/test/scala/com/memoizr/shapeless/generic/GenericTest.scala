package com.memoizr.shapeless.generic

import org.scalatest.{FlatSpecLike, Matchers}
import shapeless._

class GenericTest extends FlatSpecLike with Matchers {

  case class Foo(i: Int, s: String, b: Boolean)

  val fooGen = Generic[Foo]
  val foo = Foo(23, "foo", b = true)

  "case classes" should "be convertible to HList representations" in {
    val l = fooGen.to(foo)

    l shouldBe 23 :: "foo" :: true :: HNil

    val r = 13 :: l.tail

    val newFoo = fooGen.from(r)
    newFoo shouldBe Foo(13, "foo", true)
  }

  import poly._

  sealed trait Tree[T]

  case class Leaf[T](t: T) extends Tree[T]

  case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]

  object inc extends ->((i: Int) => i + 1)

  val tree: Tree[Int] =
    Node(
      Leaf(1),
      Node(
        Leaf(2),
        Leaf(3)
      )
    )

  "polymorphic function" should "be applied to all applicable values" in {
    everywhere(inc)(tree) shouldBe
      Node(
        Leaf(2),
        Node(
          Leaf(3),
          Leaf(4)
        )
      )
  }

  case class Book(author: String, title: String, id: Int, price: Double)

  import record._

  val bookGen = LabelledGeneric[Book]
  val tapl = Book("Benjamin Pierce", "Types and Programming Languages", 262162091, 44.11)
  val rec = bookGen.to(tapl)

  "case class" should "be viewable generically" in {
    rec('price) shouldBe 44.11

    val updatedBook = bookGen.from(rec.updateWith('price)(_ + 2.0))

    updatedBook.price shouldBe 46.11

    import syntax.singleton._

    case class ExtendedBook(author: String, title: String, id: Int, price: Double, inPrint: Boolean)

    val bookExtGen = LabelledGeneric[ExtendedBook]
    val extendedBook = bookExtGen.from(rec + ('inPrint ->> true))

    extendedBook.inPrint shouldBe true
  }
}
