package com.memoizr.doobie

import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, Matchers}

class SelectingData extends FlatSpec with BeforeAndAfterEach with Matchers {
  import doobie.imports._
  import fs2.Task
  import cats.implicits._


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
      countryInsertion("USA", "Estados Unidos De America", 27835700, Some(8510700)).run |@|
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

  it should "get info about countried" in {
    val countryName =
      sql"SELECT name FROM country WHERE code = 'ESP'".query[String]
      .unique
      .transact(xa)
      .unsafeRun

    countryName shouldBe "Spain"
  }

  it should "return an option if we're not sure that the result exists in the Deeebeee" in {
    val maybeCountryName =
      sql"SELECT name FROM country WHERE code = 'ITA'"
      .query[String]
      .option
      .transact(xa)
      .unsafeRun

    maybeCountryName shouldBe None
  }

  it should "accumulate results into a list when there's like more than one result init" in {
    val countryNames =
      sql"SELECT name FROM country ORDER BY population"
      .query[String]
      .list
      .transact(xa)
      .unsafeRun

    countryNames.head shouldBe "Estados Unidos De America"
  }

  it should "allow to retrieve a stream of results as opposed to a crappy blob" in {
    val countryNames =
      sql"SELECT name FROM country ORDER BY name"
      .query[String]
      .process
      .take(3)
      .list
      .transact(xa)
      .unsafeRun

    countryNames.size shouldBe 3
  }

}
