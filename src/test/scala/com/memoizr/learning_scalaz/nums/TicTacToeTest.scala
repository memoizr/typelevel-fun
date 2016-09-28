package com.memoizr.learning_scalaz.nums

import com.memoizr.learning_scalaz._
import org.scalatest.{FlatSpec, FlatSpecLike, Matchers}

class TicTacToeTest extends FlatSpecLike with Matchers {

  import com.memoizr.learning_scalaz.Winner._

  val game = new Game[N, N, N,
    N, N, N,
    N, N, N]()

  it should "play first move" in {
    val (newGame, winner) = game.play
      [X, N, N,
        N, N, N,
        N, N, N]

    val res = Winner.getWinner(winner).result
    res.isInstanceOf[X] shouldBe false
    res.isInstanceOf[O] shouldBe false
  }

  it should "two items are not enough" in {
    val (newGame, winner) = game.play
      [X, X, N,
        N, N, N,
        N, N, N]

    val res = Winner.getWinner(winner).result
    res.isInstanceOf[X] shouldBe false
    res.isInstanceOf[O] shouldBe false
  }

  it should "play winning move" in {
    val (newGame, winner) = game.play
      [X, X, X,
        N, N, N,
        N, N, N]

    val res = Winner.getWinner(winner).result
    res.isInstanceOf[X] shouldBe true
  }

  it should "play winning move, scond row" in {
    val (newGame, winner) = game.play
      [N, N, N,
        X, X, X,
        N, N, N]

    val res = Winner.getWinner(winner).result
    res.isInstanceOf[X] shouldBe true
  }

  it should "play winning move, third row" in {
    val (newGame, winner) = game.play
      [N, N, N,
        N, N, N,
        X, X, X]

    val res = Winner.getWinner(winner).result
    res.isInstanceOf[X] shouldBe true
  }

  it should "play winning move, first column" in {
    val (newGame, winner) = game.play
      [X, N, N,
        X, N, N,
        X, N, N]

    import Winner._
    val res = Winner.getWinner(winner).result
    res.isInstanceOf[X] shouldBe true
  }

  it should "play winning move, second column" in {
    val (newGame, winner) = game.play
      [N, X, N,
        N, X, N,
        N, X, N]

    import Winner._
    val res = Winner.getWinner(winner).result
    res.isInstanceOf[X] shouldBe true
  }

  it should "play winning move, third column" in {
    val (newGame, winner) = game.play
      [N, N, X,
        N, N, X,
        N, N, X]

    import Winner._
    val res = Winner.getWinner(winner).result
    res.isInstanceOf[X] shouldBe true
  }

  "X" should "be equal to X" in {
    Bool.toBoolean[X#isEq[X]] shouldBe true
  }

  "O" should "be equal to O" in {
    Bool.toBoolean[O#isEq[O]] shouldBe true
  }

  "X and Y" should "not be equal" in {
    Bool.toBoolean[X#isEq[O]] shouldBe false
  }
}

