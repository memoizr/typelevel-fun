package com.memoizr.doobie

import cats.data.NonEmptyList
import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

class ParametrizedQueries extends FlatSpec with BeforeAndAfterEach with Matchers {
  import doobie.imports._
  import doobie.util.query.Query0
  import fs2.Task
  import cats.implicits._

  val createCountryTable = sql"""CREATE TABLE country(
    code CHARACTER (3) NOT NULL,
    name TEXT NOT NULL,
    population INTEGER NOT NULL,
    gnp NUMERIC (10, 2)
    )""".update
  val dropCountry = sql"DROP TABLE IF EXISTS country".update

  def countryInsertion(code: String, name: String, population: Int, gnp: Option[Int]) =
    sql"INSERT INTO country (code, name, population, gnp) VALUES ($code, $name, $population, $gnp)".update

  val seed =
    countryInsertion("DEU", "Germany", 82164700, Some(2133367)).run |@|
      countryInsertion("FRA", "France", 59225700, Some(1424285)).run |@|
      countryInsertion("GBR", "Disunited Kingdom", 59623400, Some(1478330)).run |@|
      countryInsertion("USA", "Estados Unidos De America", 278357000, Some(8510700)).run |@|
      countryInsertion("ESP", "Spain", 39441700, None).run

  val xa: Transactor[Task] = DriverManagerTransactor[Task](
    driver = "org.postgresql.Driver",
    url = "jdbc:postgresql://0.0.0.0:5432",
    user = "postgres",
    pass = ""
  )

  override protected def beforeEach(): Unit = {
    (dropCountry.run *> createCountryTable.run).transact(xa).unsafeRun

    seed.map { case _ => 1 }.transact(xa).unsafeRun
  }


  case class Country(code: String, name: String, population: Int, gnp: Option[Int])

  def biggerThan(minPop: Int) =
    sql"""
      SELECT code, name, population, gnp
      FROM country
      WHERE population > $minPop
      ORDER BY population ASC
      """.query[Country]

  it should "work with parametrized queries" in {
    val namesOfCountries = biggerThan(59700000)
      .list
      .transact(xa)
      .unsafeRun
      .map(_.name)

    println(namesOfCountries)
    namesOfCountries shouldBe List("Germany", "Estados Unidos De America")
  }


  it should "work with multiple parameters" in {
    def populationIn(range: Range): Query0[Country] =
      sql"""
      SELECT code, name, population, gnp
      FROM country
      WHERE population > ${range.min} AND population < ${range.max}
      ORDER BY population ASC""".query[Country]

    val countriesName = populationIn(25000000 to 75000000)
      .list
      .transact(xa)
      .unsafeRun
      .map(_.code)

    countriesName shouldBe List("ESP", "FRA", "GBR")
  }

  it should "offer a nice syntax for IN queries" in {
    def populationIn(range: Range, codes: NonEmptyList[String]) = {
      implicit val codesParam = Param.many(codes)
      sql"""
        SELECT code, name, population, gnp
        FROM country
        WHERE population > ${range.min}
        AND population < ${range.max}
        AND code IN (${codes: codes.type})
        """.query[Country]
    }

    val countriesName = populationIn(25000000 to 75000000, NonEmptyList.of("ESP", "USA", "FRA"))
      .list
      .transact(xa)
      .unsafeRun
      .map(_.code)

    countriesName shouldBe List("FRA", "ESP")
  }
}
