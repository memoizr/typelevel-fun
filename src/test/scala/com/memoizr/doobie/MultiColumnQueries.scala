package com.memoizr.doobie

import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}
import shapeless.record.Record

class MultiColumnQueries extends FlatSpec with BeforeAndAfterEach with Matchers {

  import doobie.imports._
  import fs2.Task
  import cats.implicits._
  import shapeless._

  val create = sql"""CREATE TABLE country(
    code CHARACTER (3) NOT NULL,
    name TEXT NOT NULL,
    population INTEGER NOT NULL,
    gnp NUMERIC (10, 2)
    )""".update

  val drop = sql"DROP TABLE IF EXISTS country".update

  def countryInsertion(code: String, name: String, population: Int, gnp: Option[Int]) =
    sql"INSERT INTO country (code, name, population, gnp) VALUES ($code, $name, $population, $gnp)".update

  val seed =
    countryInsertion("DEU", "Germany", 82164700, Some(2133367)).run |@|
      countryInsertion("FRA", "France", 59225700, Some(1424285)).run |@|
      countryInsertion("GBR", "Disunited Kingdom", 59623400, Some(1478330)).run |@|
      countryInsertion("USA", "United States of America", 27835700, Some(8510700)).run |@|
      countryInsertion("ESP", "Spain", 39441700, None).run

  val xa: Transactor[Task] = DriverManagerTransactor[Task](
    driver = "org.postgresql.Driver",
    url = "jdbc:postgresql://0.0.0.0:5432",
    user = "postgres",
    pass = ""
  )

  override protected def beforeEach(): Unit = {
    (drop.run *>  create.run).transact(xa).unsafeRun

    seed.map { case _ => 1 }.transact(xa).unsafeRun
  }

  it should "allow for multiple column selection" in {
    val (name, population, gnp) =
      sql"SELECT name, population, gnp FROM country WHERE code = 'ESP'"
      .query[(String, Int, Option[Int])]
      .unique
      .transact(xa)
      .unsafeRun

    name shouldBe "Spain"
  }

  it should "support crazy HLists as well!" in {
    type CountryHListType = String :: Int :: Option[Int] :: HNil

    val hlist: CountryHListType =
      sql"SELECT name, population, gnp FROM country WHERE code = 'FRA'"
      .query[CountryHListType]
      .unique
      .transact(xa)
      .unsafeRun

    hlist.head shouldBe "France"
  }

  it should "support unbelievable Shapeless Records too!" in {
    type Rec = Record.`'name -> String, 'pop -> Int, 'gnp -> Option[Int]`.T

    val record: Rec =
      sql"SELECT name, population, gnp FROM country WHERE code = 'USA'"
      .query[Rec]
      .unique
      .transact(xa)
      .unsafeRun

    record.head shouldBe "United States of America"
  }

  it should "support case classes" in {
    case class Country(code: String, name: String, population: Long, gnp: Option[Int])

    val country: Country =
      sql"SELECT code, name, population, gnp FROM country WHERE name = 'France'"
        .query[Country]
        .unique
        .transact(xa)
        .unsafeRun

    country.code shouldBe "FRA"
  }

  case class Code(code: String)
  case class CountryInfo(name: String, pop: Int, gnp: Option[Int])

  it should "be possible to compose product types" in {
    val (code, country) =
      sql"SELECT code, name, population, gnp FROM country WHERE code = 'ESP'"
      .query[(Code, CountryInfo)]
      .unique
      .transact(xa)
      .unsafeRun

    country.name shouldBe "Spain"
  }

  it should "support maps" in {
    val notFoundCountry = CountryInfo("Not Found", 0, None)

    val countriesMap: Map[Code, CountryInfo] =
      sql"SELECT code, name, population, gnp FROM country"
      .query[(Code, CountryInfo)]
      .list
      .transact(xa)
      .unsafeRun
      .toMap

    countriesMap.getOrElse(Code("DEU"), notFoundCountry).name shouldBe "Germany"
    countriesMap.get(Code("ITA")) shouldBe None
  }
}
