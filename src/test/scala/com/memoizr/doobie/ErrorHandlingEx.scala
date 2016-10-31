package com.memoizr.doobie

import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

class ErrorHandlingEx extends FlatSpec with BeforeAndAfterEach with Matchers {
  import doobie.imports._
  import fs2.Task
  import cats.implicits._
  import cats.data.Xor

  val dropPersonTable = sql"""DROP TABLE IF EXISTS personEH""".update
  val createPersonTable = sql"""CREATE TABLE IF NOT EXISTS personEH (
    id SERIAL UNIQUE,
    name VARCHAR NOT NULL UNIQUE,
    age INT)""".update

  val xa: Transactor[Task] = DriverManagerTransactor[Task](
    driver = "org.postgresql.Driver",
    url = "jdbc:postgresql://0.0.0.0:5432",
    user = "postgres",
    pass = ""
  )

  override protected def beforeEach(): Unit = {
    (dropPersonTable.run *> createPersonTable.run).transact(xa).unsafeRun
  }

  def insert(n: String, a: Option[Int]): ConnectionIO[Long] =
    sql"""INSERT INTO personeh (name, age) VALUES ($n, $a)""".update.withUniqueGeneratedKeys("id")


  it should "catch an exception" in {
    def safeInsert(name: String, age: Option[Int]): ConnectionIO[Either[String, Long]] =
      insert(name, age).attemptSome {
        case e: java.sql.SQLException => "Ooops!"
      }
    val insertedRows = for {
      john <- safeInsert("John", Some(23))
      otherJohn <- safeInsert("John", Some(30))
    } yield otherJohn

    val result = insertedRows.transact(xa).unsafeRun

    result shouldBe Left("Ooops!")
  }

  val UNIQUE_VIOLATION = SqlState("23505")

  it should "catch a particular exception" in {
    def safeInsert(name: String, age: Option[Int]): ConnectionIO[Either[String, Long]] =
      insert(name, age)
      .attemptSomeSqlState {
        case UNIQUE_VIOLATION => "Unique Key Violation!"
        case _ => "Another error"
      }

    val insertedRows = for {
      john <- safeInsert("John", Option(35))
      otherJohn <- safeInsert("John", Option(20))
    } yield otherJohn

    val result = insertedRows.transact(xa).unsafeRun

    result shouldBe Left("Unique Key Violation!")
  }

  it should "recover from an error" in {
    def safeInsert(name: String, age: Option[Int]) =
      insert(name, age)
          .exceptSqlState {
//        case SqlState(_) => insert("bars_20", age)
        case _ => insert("foostBarsss", age)
      }
//        .attemptSomeSqlState {
//          case UNIQUE_VIOLATION => "Unique Key Violation!"
//          case _ => "Another error"
//        }

    case class Person(name: String, age: Option[Int])

    def findPersonById(id: Long) = {
      sql"SELECT name, age FROM personeh WHERE (id=$id)".query[Person].unique
    }

    val insertedRows = for {
      john <- safeInsert("John" , Option(34))
      otherJohn <- safeInsert("John", Option(20))
//      info <- findPersonById(otherJohn)
    } yield otherJohn

    val result = insertedRows.transact(xa).unsafeRun

    println(result)
//    result.name shouldBe "John_20"
//    result.age shouldBe Some(20)
  }
}
