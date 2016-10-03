package com.memoizr.shapeless.lenses

import org.scalatest.{FlatSpecLike, Matchers}

class LensesEx extends FlatSpecLike with Matchers {
  import shapeless._

  case class Address(street: String, city: String, postcode: String)
  case class Person(name: String, age: Int, address: Address)


  val nameLens = lens[Person] >> 'name
  val ageLens = lens[Person] >> 'age
  val addressLens = lens[Person] >> 'address
  val streetLens = lens[Person] >> 'address >> 'street
  val cityLens = lens[Person] >> 'address >> 'city
  val postcodeLens = lens[Person] >> 'address >> 'postcode
  val person = Person("Joe", 37, Address("Street", "City", "Postcode"))

  "a lens" should "read a field" in {
    ageLens.get(person) shouldBe 37
  }

  it should "update a field" in {
    val updatedPerson = ageLens.set(person)(38)
    updatedPerson.age shouldBe 38
  }

  it should "transform a field" in {
    val updatedPerson = ageLens.modify(person)(_ + 1)
    updatedPerson.age shouldBe 38
  }

  it should "read a nested field" in {
    streetLens.get(person) shouldBe "Street"
  }

  it should "update a nested field" in {
    val updatedPerson = streetLens.set(person)("Montpelier Road")
    updatedPerson.address.street shouldBe "Montpelier Road"
  }
}
