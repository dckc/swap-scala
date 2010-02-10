package org.w3.swap.test

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import org.w3.swap
import swap.logic.Variable
import swap.logic.Formula // TODO: move to logic1?
import swap.sexp
import swap.logic1.{Appeal, CoreProof, Equal, ComputationalLogic}

// move these to core logic?
class Var(s: Symbol) extends Variable {
  def quote() = sexp.Atom(s)
}

class PfCheck1 extends Spec with ShouldMatchers {
  describe("Core proof checker") {
    it("should...@@") {
      val log = new ComputationalLogic(Map()) // no function symbols
      val vx = new Var('x)
      val vy = new Var('y)
      val vz = new Var('y)
      var f = Equal(vx, vy)
      var g = log.Or(Equal(vx, vz), f)
      var pf = List(Appeal('EXPANSION, g,
			   List(Appeal('THEOREM, f, Nil, Nil)),
			   Nil))
      CoreProof.proofp(pf, Nil, List(f), log) should equal (true)
    }
  }
}
