package com.memoizr.learning_scalaz


trait N extends Cell {
  type isX[ifX <: Bool, ifO <: Bool] = False
  type isEq[A <: Cell] <: False
  type isValid <: False
  type validate = Cell
}

trait Cell {
  type isX[ifX <: Bool, ifO <: Bool] <: Bool
  type isEq[A <: Cell] <: Bool
  type isValid <: Bool
  type validate <: Cell
}

trait X extends Cell {
  type isX[ifX <: Bool, ifO <: Bool] = ifX
  type isEq[A <: Cell] = A#isX[True, False]
  type isValid <: True
  type validate = X
}

trait O extends Cell {
  type isX[ifX <: Bool, ifO <: Bool] = ifO
  type isEq[A <: Cell] = A#isX[False, True]
  type isValid <: True
  type validate = O
}

class Game[
A <: Cell, B <: Cell, C <: Cell,
D <: Cell, E <: Cell, F <: Cell,
G <: Cell, H <: Cell, I <: Cell] {

  def play[
  A1 <: A#validate, B1 <: B#validate, C1 <: C#validate,
  D1 <: D#validate, E1 <: E#validate, F1 <: F#validate,
  G1 <: G#validate, H1 <: H#validate, I1 <: I#validate
  ] = {
    type allEqual[X <: Cell, Y <: Cell, Z <: Cell] = X#isEq[Y]#AND[Y#isEq[Z]]

    (new Game[
      A1, B1, C1,
      D1, E1, F1,
      G1, H1, I1], new winner[
      allEqual[A1, B1, C1]#If[
        A1,
        allEqual[D1, E1, F1]#If[
          D1,
          allEqual[G1, H1, I1]#If[
            G1,
            allEqual[A1, D1, G1]#If[
              A1,
              allEqual[B1, E1, H1]#If[
                B1,
                allEqual[C1, F1, I1]#If[
                  C1,
                  N, Cell], Cell], Cell], Cell], Cell], Cell]])
  }
}

class winner[box <: Cell]

class cellRep[cell <: Cell](val result: Cell)

object Winner {
  implicit val nothing: cellRep[N] = new cellRep(new N() {})
  implicit val oWinner: cellRep[O] = new cellRep(new O() {})
  implicit val xWinner: cellRep[X] = new cellRep(new X() {})

  def getWinner[A <: Cell : cellRep](winner: winner[A]) = implicitly[cellRep[A]]
}
