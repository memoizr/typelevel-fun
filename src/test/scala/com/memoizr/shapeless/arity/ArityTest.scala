package com.memoizr.shapeless.arity

import org.scalatest.{FlatSpec, Matchers}
import shapeless.Generic

import scalaz.typelevel.HList

class ArityTest extends FlatSpec with Matchers {

  "function arity" should "be abstracted to an HList" in {
    import shapeless.syntax.std.function._
    import shapeless.ops.function._
    import shapeless.syntax.std.function.fnHListOps


//    def applyProduct[P <: Product, F, L <: HList, R](p: P)(f: F)(implicit gen: Generic.Aux[P, L], fp: FnToProduct.Aux[F, L => R]) = f.toProduct(gen.to(p))
  }

}
