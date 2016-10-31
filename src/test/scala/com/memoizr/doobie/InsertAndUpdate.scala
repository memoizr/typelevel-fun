package com.memoizr.doobie

import cats.free.Free
import doobie.free.connection.ConnectionOp
import doobie.imports.Update0
import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

class InsertAndUpdate extends FlatSpec with BeforeAndAfterEach with Matchers {

  import doobie.imports._
  import fs2.Task
  import cats.implicits._

  val xa: Transactor[Task] = DriverManagerTransactor[Task](
    driver = "org.postgresql.Driver",
    url = "jdbc:postgresql://0.0.0.0:5432",
    user = "postgres",
    pass = ""
  )

  val drop: Update0 = sql"DROP TABLE IF EXISTS person".update

  val create: Update0 =
    sql"""
      CREATE TABLE person (
      id SERIAL UNIQUE,
      name VARCHAR NOT NULL,
      age SMALLINT
      )
      """.update

  override protected def beforeEach(): Unit = {
    (drop.run *> create.run).transact(xa).unsafeRun
  }

  it should "be able to update the schema of the db" in {
    //    (create.run).transact(xa).unsafeRun
  }

  def insert1(name: String, age: Option[Short]): Update0 =
    sql"INSERT INTO person (name, age) VALUES ($name, $age)".update

  it should "insert a row" in {
    val insertedRows =
      insert1("Marie", Option(38))
        .run
        .transact(xa)
        .unsafeRun

    println(insertedRows)
  }

  it should "insert several rows at once" in {
    val rows = for {
      row1 <- insert1("Alice", Option(12)).run
      row2 <- insert1("Bob", None).run
      row3 <- insert1("Johnny", Option(17)).run
    } yield (row1, row2, row3)

    val insertedRows = rows.transact(xa).unsafeRun

    insertedRows shouldBe(1, 1, 1)
  }

  it should "allow for for comprehension when adding multiple rows to the deebee" in {
    val rows = for {
      row1 <- insert1("Alice", Option(12)).run
      row2 <- insert1("Bob", None).run
      row3 <- insert1("John", Option(17)).run
    } yield row1 + row2 + row3

    val insertedRows = rows transact xa unsafeRun

    insertedRows shouldBe 3
  }

  it should "be better to use an applicative functor whenever there's no dependency between tasks" in {
    val transactions = insert1("Alice", Option(234)).run |@|
      insert1("Bobcats", Option(0)).run |@|
      insert1("Jerry's", Option(11)).run

    val insertedRows = transactions.map {
      _ + _ + _
    }.transact(xa).unsafeRun

    insertedRows shouldBe 3
  }

  it should "allow to traverse a list of transactions" in {
    val people = List[(String, Option[Short])](
      ("Kate", Option(12)),
      ("Rob", Option(234)),
      ("Phtephen", Option(234)),
      ("Slob", Option(33))
    )

    val insertedRows = people.traverse[ConnectionIO, Int](item => (insert1 _).tupled(item).run).transact(xa).unsafeRun.foldLeft(0)(_ + _)

    insertedRows shouldBe 4
  }

  case class Person(id: Int, name: String, age: Option[Int])

  it should "allow to update an entry" in {
    val result: Free[ConnectionOp, (Int, Int, List[Person])] = for {
      insertedRows <- insert1("AliceInsert", Option(143)).run
      updatedRows <- sql"update person set age = 15 where name = 'AliceInsert'".update.run
      persons <- sql"SELECT id, name, age FROM person WHERE name = 'AliceInsert'".query[Person].list
    } yield (insertedRows, updatedRows, persons)

    val (insertedRows, updatedRows, person) = result.transact(xa).unsafeRun

    insertedRows shouldBe 1
    //updatedRows shouldBe 3
    person.head.age shouldBe Some(15)
  }

  // RETRIEVING INFO

  def insert2(name: String, age: Option[Short]): ConnectionIO[Person] =
    for {
      _ <- sql"INSERT INTO person (name, age) VALUES ($name, $age)".update.run
      id <- sql"SELECT LASTVAL()".query[Long].unique
      p <- sql"SELECT id, name, age FROM person WHERE id = $id".query[Person].unique
    } yield p

  def insert2_h2(name: String, age: Option[Int]): ConnectionIO[Person] = for {
    id <- sql"INSERT INTO person (name, age) VALUES ($name, $age)".update.withUniqueGeneratedKeys[Int]("id")
    p <- sql"SELECT id, name, age, FROM person WHERE id = $id".query[Person].unique
  } yield p


  it should "allow to retrieve stored info" in {
    val person = insert2("Ramone", Option(42)).transact(xa).unsafeRun

    person.name shouldBe "Ramone"
    person.age shouldBe Option(42)
  }

  // POSTGRESQL provides a way to return multiple specified columns from the inserted row"

  def insert3(name: String, age: Option[Int]): ConnectionIO[Person] = {
    sql"INSERT INTO person (name, age) VALUES ($name, $age)".update.withUniqueGeneratedKeys("id", "name", "age")
  }

  it should "allow for batch updates" in {
    type PersonInfo = (String, Option[Short])

    def insertMany(ps: List[PersonInfo]): ConnectionIO[Int] = {
      val sql = "INSERT INTO person (name, age) VALUES (?, ?)"
      Update[PersonInfo](sql).updateMany(ps)
    }

    val data = List[PersonInfo](
      ("Frank", Some(12)),
      ("Daddy", None)
    )

    val insertedRows = insertMany(data).transact(xa).unsafeRun

    insertedRows shouldBe 2
  }

}
