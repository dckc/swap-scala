package org.w3.swap.test

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import org.w3.swap
import swap.sexp
import swap.logic0.{Or, Not, Appeal}
import swap.logic1eq.{Equal}
import swap.logic1c.{Var, History}

class PfCheck1 extends Spec with ShouldMatchers {
  implicit def symvar(s: Symbol) = Var(s)

  describe("Core proof checker") {
    // no functions, no axioms
    val h = new History(Map(), Nil)

    var a = Equal('x, 'y)
    var b = Equal('x, 'z)

    it("should... accept a proof of A or B from B") {
      var pfb = List(Appeal('THEOREM, b, Nil, Nil))
      var step = Appeal('EXPANSION, Or(a, b), pfb, Nil)
      h.proofp(List(step), List(b)) should equal (true)
    }


    it("should *not* accept a proof of A or B from A") {
      var pfa = List(Appeal('THEOREM, a, Nil, Nil))
      var step = Appeal('EXPANSION, Or(a, b), pfa, Nil)
      h.proofp(List(step), List(a)) should equal (false)
    }
  }
}
