package com.memoizr.shapeless.lazyex

import org.scalatest.{FlatSpecLike, Matchers}

class LazyEx extends FlatSpecLike with Matchers {
  "lazy" should "work" in {
    val l: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))

    show(l) shouldBe "Cons(1, Cons(2, Cons(3, Nil)))"
  }

  import shapeless._

  sealed trait List[+T]

  case class Cons[T](hd: T, tl: List[T]) extends List[T]

  sealed trait Nil extends List[Nothing]

  case object Nil extends Nil

  trait Show[T] {
    def apply(t: T): String
  }

  def aShow[T](string: T => String) = new Show[T] {
    override def apply(t: T): String = string(t)
  }

  object Show {
    implicit def showInt: Show[Int] = aShow {
      _.toString
    }

    implicit def showNil: Show[Nil] = aShow[Nil] { _ =>
      "Nil"
    }

    implicit def showCons[T](implicit st: Lazy[Show[T]], sl: Lazy[Show[List[T]]]): Show[Cons[T]] = aShow[Cons[T]] { t =>
      s"Cons(${show(t.hd)(st.value)}, ${show(t.tl)(sl.value)})"
    }

    implicit def showList[T](implicit sc: Lazy[Show[Cons[T]]]): Show[List[T]] = aShow[List[T]] {
      case n: Nil => show(n)
      case c: Cons[T] => show(c)(sc.value)
    }

  }

  def show[T](t: T)(implicit s: Show[T]) = s(t)
}
