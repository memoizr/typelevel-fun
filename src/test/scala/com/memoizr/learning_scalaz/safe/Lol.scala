package com.memoizr.learning_scalaz.safe

import org.scalatest.FlatSpec
import scalaz.OptionT

class Lol extends FlatSpec {
  println(performOperation(1, 3, add))

  def add(x: Int, y: Int) = x + y

  def performOperation(x: Int, y: Int, operation: (Int, Int) => Int) = operation(x, y)
}
