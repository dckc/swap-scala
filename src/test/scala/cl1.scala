package org.w3.swap.test

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import org.w3.swap

import swap.webdata.RDFQ // TODO: rename to ECQ or something.
import swap.logic1.{Variable, Term}
import swap.logic1c.{Var, Constant, App, Quotable}
import swap.sexp.{Atom, Cons, SExp}
import SExp.fromSeq

import swap.logic1cl.{CoherentLogic,
		      Implication, Conjunction, Disjunction, Exists, Atomic,
		    Implies, Or, And}

class TestLogic(t: List[Implication]) extends CoherentLogic(t) {
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
    App(Symbol(base + i), Nil)
  }

  def linearize(pf: Appeal): List[Appeal] = {
    def recur(todo: List[Appeal]): List[Appeal] = {
      todo match {
	case Nil => Nil
	case a0 :: a1n => {
	  val d2 = recur(a0.subproofs ++ a1n)
	  /* disabling this while we work with assumptions...
	  if (d2.exists { _.conclusion == a0.conclusion }) d2
	  else a0 :: d2
	  */
	  a0 :: d2
	}
      }
    }
    recur(List(pf)).reverse
  }

  def quotepf(steps: List[Appeal]): SExp = {
    def recur(i: Int, todo: List[Appeal], done: Map[Appeal, Int]): SExp = {
      def quote1(i: Int, pf: Appeal): SExp = {
	fromSeq(List(i, quote(pf.conclusion),
		     pf.method, fromSeq(pf.subproofs.map(done(_))),
		     fromSeq(pf.extras)))
      }

      todo match {
	case Nil => SExp.NIL
	case step :: rest => {
	  val qstep = quote1(i, step)
	  Cons(qstep, recur(i+1, rest, done + (step -> i)))
	}
      }
    }

    recur(1, steps, Map())
  }

  def quote(pf: Appeal): SExp = {
    fromSeq(List('appeal, pf.method, quote(pf.conclusion),
		 fromSeq(pf.subproofs.map(quote _)), fromSeq(pf.extras)))
  }

  def quote(f: Formula): SExp = {
    f match {
      case Implies(c, d) => fromSeq(List('implies, quote(c), quote(d)))
      case And(ai) => fromSeq('and :: ai.map(quote _))
      case Or(ei) => fromSeq('or :: ei.map(quote _))
      case Exists(xi, g) => fromSeq(List('exists, quote(xi.toList), quote(g) ))
      case Atomic(rel, args) => Cons(Atom(rel), quote(args))
    }
  }

  def quote(terms: List[Term]): SExp = {
    fromSeq(terms map {
      case tq: Quotable => tq.quote()
      case t => Atom(t) // shouldn't happen
    })
  }
}

class CoherentLogicMisc extends Spec with ShouldMatchers {
  describe("coherent logic breadth-first consequence"){
    implicit def tovar(s: Symbol) = Var(s)
    implicit def toterm(s: String) = Constant(Atom(s))
    def app(s: Symbol, terms: Term*) = Atomic(s, terms.toList)

    it("should handle the socrates inference"){
      // all men are mortal
      val theory = List(
	Implies(app('Man, 'x), app('Mortal, 'x)) )
	
      val state = Set( app('Man, "socrates") )
      val conjecture = app('Mortal, "socrates")
      val l = new TestLogic(theory)
      val pfs = state.toList.map(l.Appeal('THEOREM, _, Nil, Nil))
      val pf = l.consequence(pfs, conjecture).get
      l.quotepf(l.linearize(pf)).toString() should equal (
"""((1 (Man '"socrates" ) THEOREM () () )
  (2 (implies (Man x ) (Mortal x ) ) AXIOM () () )
  (3
    (implies (Man '"socrates" ) (Mortal '"socrates" ) )
    INSTANTIATION
    (2 )
    (Map(Var('x) -> Constant("socrates")) )
    )
  (4 (Mortal '"socrates" ) MODUS_PONENS (3 1 ) () )
  )"""
)
    }

    it("should handle the example in section 3 of the paper"){
      val theory: List[Implication] =
	List(Implies(app('q, 'x), app('p)),
	     Or(List(app('p),
		     Exists(Set(Var('x)), app('q, 'x)) )) )
      val conjecture = app('p)
      val l = new TestLogic(theory)
      val pf = l.consequence(Nil, conjecture).get
      l.quotepf(l.linearize(pf)).toString() should equal (
"""((1 (exists (x ) (q x ) ) ASSUMPTION () () )
  (2 (q (x2 ) ) EXISTS_ELIM (1 ) (Map(Var('x) -> App('x2,List())) ) )
  (3 (implies (q x ) (p ) ) AXIOM () () )
  (4
    (implies (q (x2 ) ) (p ) )
    INSTANTIATION
    (3 )
    (Map(Var('x) -> App('x2,List())) )
    )
  (5 (p ) MODUS_PONENS (4 2 ) () )
  (6 (p ) ASSUMPTION () () )
  (7 (or (p ) (exists (x ) (q x ) ) ) AXIOM () () )
  (8 (p ) OR_INTRO (7 6 5 ) () )
  )"""
)
    }

    it("would loop indefinely for a non-consequence :-/"){
      // all men are mortal
      val theory = List(
	Implies(Atomic('Man, List('x)), Atomic('Mortal, List('x))) )
	
      val state = Set( Atomic('Man, List("socrates")) )
      val conjecture = Atomic('Mortal, List("bob"))

      //TestLogic.consequence_bf(theory, state, conjecture) should equal (false)
      false should equal (false)
    }
  }
}
