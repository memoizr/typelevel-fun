package com.memoizr.learning_scalaz.nums

import com.memoizr.learning_scalaz.{Bool, False, True}
import org.scalatest.{FlatSpec, FlatSpecLike, Matchers}

class TicTacToeTest extends FlatSpecLike with Matchers {

  import Winner._

  val game = new Game[N, N, N,
    N, N, N,
    N, N, N]()

  it should "play first move" in {
    val (newGame, winner) = game.play
      [X, N, N,
        N, N, N,
        N, N, N]

    val res = Winner.getWinner(winner).result
    (res.isInstanceOf[X]) shouldBe false
    (res.isInstanceOf[O]) shouldBe false
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
    (res.isInstanceOf[X]) shouldBe true
  }

  it should "play winning move, scond row" in {
    val (newGame, winner) = game.play
      [N, N, N,
        X, X, X,
        N, N, N]

    val res = Winner.getWinner(winner).result
    (res.isInstanceOf[X]) shouldBe true
  }

  it should "play winning move, third row" in {
    val (newGame, winner) = game.play
      [N, N, N,
        N, N, N,
        X, X, X]

    val res = Winner.getWinner(winner).result
    (res.isInstanceOf[X]) shouldBe true
  }

  it should "play winning move, first column" in {
    val (newGame, winner) = game.play
      [X, N, N,
        X, N, N,
        X, N, N]

    import Winner._
    val res = Winner.getWinner(winner).result
    (res.isInstanceOf[X]) shouldBe true
  }

  it should "play winning move, second column" in {
    val (newGame, winner) = game.play
      [N, X, N,
        N, X, N,
        N, X, N]

    import Winner._
    val res = Winner.getWinner(winner).result
    (res.isInstanceOf[X]) shouldBe true
  }

  it should "play winning move, third column" in {
    val (newGame, winner) = game.play
      [N, N, X,
        N, N, X,
        N, N, X]

    import Winner._
    val res = Winner.getWinner(winner).result
    (res.isInstanceOf[X]) shouldBe true
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

trait N extends Box {
  type isX[ifX <: Bool, ifO <: Bool] = False
  type isEq[A <: Box] <: False
  type isValid <: False
  type validate = Box
}

trait Box {
  type isX[ifX <: Bool, ifO <: Bool] <: Bool
  type isEq[A <: Box] <: Bool
  type isValid <: Bool
  type validate <: Box
}

trait X extends Box {
  type isX[ifX <: Bool, ifO <: Bool] = ifX
  override type isEq[A <: Box] = A#isX[True, False]
  type isValid <: True
  type validate = X
}

trait O extends Box {
  type isX[ifX <: Bool, ifO <: Bool] = ifO
  override type isEq[A <: Box] = A#isX[False, True]
  type isValid <: True
  type validate = O
}

trait Gamable {
}

class Game
[A <: Box, B <: Box, C <: Box,
D <: Box, E <: Box, F <: Box,
G <: Box, H <: Box, I <: Box] extends Gamable {

  def play[
  A1 <: A#validate, B1 <: B#validate, C1 <: C#validate,
  D1 <: D#validate, E1 <: E#validate, F1 <: F#validate,
  G1 <: G#validate, H1 <: H#validate, I1 <: I#validate
  ] = {
    type row[X <: Box, Y <: Box, Z <: Box] = X#isEq[Y]#AND[Y#isEq[Z]]

    type help = row[D1, E1, F1]#If[D1, N, Box]

    (new Game[
      A1, B1, C1,
      D1, E1, F1,
      G1, H1, I1], new winner[
      row[A1, B1, C1]#If[
        A1,
        row[D1, E1, F1]#If[
          D1,
          row[G1, H1, I1]#If[
            G1,
            row[A1, D1, G1]#If[
              A1,
              row[B1, E1, H1]#If[
                B1,
                row[C1, F1, I1]#If[
                  C1,
                  N, Box], Box], Box], Box], Box], Box]])
  }
}

class winner[chah <: Box]

class nrep[res <: Box](val result: Box)

object Winner {
  implicit val nWinner: nrep[N] = new nrep(new N() {})
  implicit val oWinner: nrep[O] = new nrep(new O() {})
  implicit val xWinner: nrep[X] = new nrep(new X() {})

  def getWinner[A <: Box : nrep](winner: winner[A]) = implicitly[nrep[A]]
}


class victorious[chah <: Bool]

class viccy[res <: Bool](val result: Bool)

object Victorious {
  implicit val otrue: viccy[True] = new viccy(new True() {})
  implicit val xfalse: viccy[False] = new viccy(new False() {})

  def getVictorious[A <: Bool : viccy](winner: victorious[A]) = implicitly[viccy[A]]
}
