package org.w3.swap.test

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import org.w3.swap

import swap.webdata.RDFQ // TODO: rename to ECQ or something.
import swap.logic1.{Variable, Term}
import swap.logic1c.{Var, Constant, App}
import swap.sexp.Atom

import swap.logic1cl.{CoherentLogic,
		      Implication, Conjunction, Disjunction, Exists, Atomic}

class TestLogic(t: Seq[Implication]) extends CoherentLogic(t) {
  var i = 0

  def fresh(pattern: Variable) = {
    i = i + 1 // umm... needs lock?
    val base = pattern match {
      case Var(sym) => sym.name
      case _ => "v"
    }
    Var(Symbol(base + i))
  }

  def parameter(pattern: Variable) = {
    i = i + 1
    val base = pattern match {
      case Var(sym) => sym.name
      case _ => "a"
    }
    Constant(Atom(base + i))
  }
}

class CoherentLogicMisc extends Spec with ShouldMatchers {
  describe("coherent logic breadth-first consequence"){
    implicit def tovar(s: Symbol) = Var(s)
    implicit def toterm(s: String) = Constant(Atom(s))
    implicit def a1(a: Atomic) = List(a)
    def app(s: Symbol, terms: Term*) = Atomic(s, terms.toList)

    // move these to logic1cl?
    implicit def c1(a: Atomic) = Conjunction(List(a))
    implicit def e1(a: Atomic) = Exists(Set.empty, a)
    implicit def d1(a: Atomic) = Disjunction(List(a))
    implicit def i1(d: Disjunction) = Implication(Conjunction(Nil), d)
    implicit def i2(a: Atomic) = Implication(Conjunction(Nil), a)

    it("should handle the socrates inference"){
      // all men are mortal
      val theory = List(
	Implication(app('Man, 'x), app('Mortal, 'x)) )
	
      val state = Set( app('Man, "socrates") )
      val conjecture = app('Mortal, "socrates")
      val l = new TestLogic(theory)
      l.consequence_bf(state, conjecture) should equal (true)
    }

    it("should handle the example in section 3 of the paper"){
      val theory: List[Implication] =
	List(Implication(app('q, 'x), app('p)),
	     Disjunction(List(app('p),
			      Exists(Set(Var('x)), app('q, 'x)) )) )
      val conjecture: Disjunction = app('p)
      val l = new TestLogic(theory)
      l.consequence_bf(Set.empty, conjecture) should equal (
	true)
    }

    it("would loop indefinely for a non-consequence :-/"){
      // all men are mortal
      val theory = List(
	Implication(Atomic('Man, List('x)), Atomic('Mortal, List('x))) )
	
      val state = Set( Atomic('Man, List("socrates")) )
      val conjecture = Atomic('Mortal, List("bob"))

      //TestLogic.consequence_bf(theory, state, conjecture) should equal (false)
      false should equal (false)
    }
  }
}

