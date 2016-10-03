package com.memoizr.serializer

import org.scalatest.{FlatSpecLike, Matchers}
import shapeless.LabelledGeneric
import shapeless.labelled.FieldType


class ParserTest extends FlatSpecLike with Matchers {

  case class SimpleWrapper(value: Int)

  case class ZeroWrapper()

  implicit class Enhancer[T: JsonEncoder](a: T) {
    implicit def encode: JsonEncoder[T]#Out = implicitly[JsonEncoder[T]].encode(a)
  }

//  implicit class JsonSerializer[T : JsonSerial](a: T) {
//    implicit def toJson = implicitly[JsonSerial[T]].toJson(a)
//  }

  import com.memoizr.serializer.Serializer._

  it should "encode an empty object" in {

    ZeroWrapper.encode shouldBe JsonObject(List())
  }

  it should "encode an wrapper of single int" in {
    SimpleWrapper(1).encode shouldBe JsonObject(List(("value", JsonInt(1))))
  }

  def foo[A, B <: JsonValue](a: A)(implicit ev: JsonEncoder.Aux[A, B], serial: LabelledGeneric[A]) = ev.encode(a).toString

  it should "serialize a JsonObject to string" in {
    println(foo(SimpleWrapper(1)))
    case class DoubleWrapper(a: Int, b: Int)
    case class NestedWrapper(a: Int, b: DoubleWrapper)
    println(foo(NestedWrapper(1, DoubleWrapper(2, 3))))
//    SimpleWrapper(1).encode.toJson shouldBe """{"value": 1}"""
  }

  implicit val jsonObject: JsonSerial[JsonObject] = new JsonSerial[JsonObject] {
    override def toJson(value: JsonObject): String = value.toString
  }
}


trait JsonSerial[V <: JsonValue] {
  def toJson(value: V): String
}

