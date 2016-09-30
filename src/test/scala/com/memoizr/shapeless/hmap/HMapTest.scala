package com.memoizr.shapeless.hmap

import org.scalatest.{FlatSpecLike, Matchers}
import shapeless._
import ops.hlist._

class HMapTest extends FlatSpecLike with Matchers {
  object  Helper {
    class BiMapIS[K, V]

    implicit val intToString = new BiMapIS[Int, String]
    implicit val stringToInt = new BiMapIS[String, Int]

    val hm = HMap[BiMapIS](23 -> "foo", "bar" -> 13)
  }

  import Helper._

  "heterogeneous maps" should "provide arbitrary relation between key and value type" in {

//    val hm2 = HMap[BiMapIS](23 -> "foo", 23 -> 13) // doesn't compile

    hm.get(23) shouldBe Some("foo")
    hm.get("bar") shouldBe Some(13)
  }

  it should "be possible to view it as a polymorphic function value" in {
    import hm._

    val hList = 23 :: "bar" :: HNil
    val m = hList map hm

    m shouldBe("foo"::13::HNil)
  }
}
