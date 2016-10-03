package com.memoizr.serializer

trait JsonValue

final case class JsonInt(value: Int) extends JsonValue

final case class JsonDouble(value: Double) extends JsonValue

final case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue


trait JsonObjectEncoder[A] extends JsonEncoder[A] {
  type Out = JsonObject
  override def encode(value: A): Out
}

trait JsonEncoder[A] {
  type Out <: JsonValue

  def encode(value: A): Out
}

object JsonEncoder {
  type Aux[A, Out0 <: JsonValue] = JsonEncoder[A] {type Out = Out0}
}

object Serializer {

  import shapeless.LabelledGeneric
  import shapeless.labelled.FieldType

  import shapeless.{HList, ::, HNil, Lazy, Witness}


  implicit val intEncoder: JsonEncoder[Int] = createEncoder(num => JsonInt(num))

  implicit val doubleEncoder: JsonEncoder[Double] = createEncoder(num => JsonDouble(num))


  implicit def genericObjectEncoder[A, H <: HList](implicit generic: LabelledGeneric.Aux[A, H],
                                                   hEncoder: Lazy[JsonObjectEncoder[H]]): JsonEncoder[A] =
    createObjectEncoder(value => hEncoder.value.encode(generic.to(value)))

  implicit def genericObjectEncoders[A, H <: HList](implicit generic: LabelledGeneric.Aux[A, H],
                                                   hEncoder: Lazy[JsonObjectEncoder[H]]): JsonEncoder.Aux[A, JsonObject] =
    createObjectEncoder(value => hEncoder.value.encode(generic.to(value)))

  implicit val hNilEncoder: JsonObjectEncoder[HNil] = createObjectEncoder(_ => JsonObject(Nil))

  implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](implicit witness: Witness.Aux[K],
                                                              hEncoder: Lazy[JsonEncoder[H]],
                                                              tEncoder: JsonObjectEncoder[T]
                                                             ): JsonObjectEncoder[FieldType[K, H] :: T] = {
    val fieldName: String = witness.value.name
    createObjectEncoder { hlist =>
      val head = hEncoder.value.encode(hlist.head)
      val tail = tEncoder.encode(hlist.tail)
      JsonObject((fieldName, head) :: tail.fields)
    }
  }

  def createObjectEncoder[A](func: A => JsonObject): JsonObjectEncoder[A] = new JsonObjectEncoder[A] {
    override def encode(value: A): JsonObject = func(value)
  }

  def createEncoder[A, B <: JsonValue](func: A => B): JsonEncoder[A] = new JsonEncoder[A] {
    override type Out = B
    override def encode(value: A): Out = func(value)
  }
}
