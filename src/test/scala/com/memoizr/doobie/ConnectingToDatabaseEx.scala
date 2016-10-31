package com.memoizr.doobie

import java.sql.DriverManager
import java.util.Properties

import cats.Applicative
import org.scalatest.{FlatSpec, Matchers}

class ConnectingToDatabaseEx extends FlatSpec with Matchers {

  import doobie.imports._
  import fs2.Task
  import cats.implicits._

  val xa = DriverManagerTransactor[Task](
    driver = "org.postgresql.Driver",
    url = "jdbc:postgresql://0.0.0.0:5432",
    user = "postgres",
    pass = ""
  )

  it should "perform a trivial query" in {
    Applicative[ConnectionIO].pure(42).transact(xa).unsafeRun shouldBe 42

    sql"select 42".query[Int].unique.transact(xa).unsafeRun shouldBe 42
  }

  it should "perform a more complex query" in {
    val largerProgram = for {
      a <- sql"select 42".query[Int].unique
      b <- sql"select power(5,2)".query[Int].unique
    } yield (a, b)

    largerProgram.transact(xa).unsafeRun shouldBe (42, 25)
  }

  it should "be possible to do the above with an applicative" in {
    val oneProgram: ConnectionIO[Int] = sql"select 42".query[Int].unique
    val anotherProgram: ConnectionIO[Int] = sql"select power(5, 2)".query[Int].unique

    (oneProgram |@| anotherProgram).map { _ + _ }.transact(xa).unsafeRun shouldBe 67
  }
}