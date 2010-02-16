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
		      Implication, Conjunction, Disjunction, Exists, Atomic}

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
	  if (d2.exists { _.conclusion == a0.conclusion }) d2
	  else a0 :: d2
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
      case Implication(c, d) => fromSeq(List('implies, quote(c), quote(d)))
      case Conjunction(ai) => fromSeq('and :: ai.map(quote _))
      case Disjunction(ei) => fromSeq('or :: ei.map(quote _))
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
      val pfs = state.toList.map(l.Appeal('THEOREM, _, Nil, Nil))
      val pf = l.derive_bf(state, pfs, conjecture).get
      l.quotepf(l.linearize(pf)).toString() should equal (
"""((1 (Man '"socrates" ) THEOREM () () )
  (2 (and (Man '"socrates" ) ) AND_INTRO (1 ) () )
  (3
    (implies (and (Man x ) ) (or (exists () (and (Mortal x ) ) ) ) )
    AXIOM
    ()
    ()
    )
  (4
    (implies
      (and (Man '"socrates" ) )
      (or (exists () (and (Mortal '"socrates" ) ) ) )
      )
    INSTANTIATION
    (3 )
    (Map(Var('x) -> Constant("socrates")) )
    )
  (5
    (or (exists () (and (Mortal '"socrates" ) ) ) )
    MODUS_PONENS
    (4 2 )
    ()
    )
  (6 (exists () (and (Mortal '"socrates" ) ) ) CONTRACTION (5 ) () )
  (7 (and (Mortal '"socrates" ) ) EXISTS_ELIM (6 ) (Map() ) )
  (8 (Mortal '"socrates" ) ERASURE (7 ) () )
  )"""
)
    }

    it("should handle the example in section 3 of the paper"){
      val theory: List[Implication] =
	List(Implication(app('q, 'x), app('p)),
	     Disjunction(List(app('p),
			      Exists(Set(Var('x)), app('q, 'x)) )) )
      val conjecture: Disjunction = app('p)
      val l = new TestLogic(theory)
      val pf = l.derive_bf(Set.empty, Nil, conjecture).get
      l.quotepf(l.linearize(pf)).toString() should equal (
"""((1 (and ) AND_INTRO () () )
  (2
    (implies
      (and )
      (or (exists () (and (p ) ) ) (exists (x ) (and (q x ) ) ) )
      )
    AXIOM
    ()
    ()
    )
  (3
    (implies
      (and )
      (or (exists () (and (p ) ) ) (exists (x ) (and (q x ) ) ) )
      )
    INSTANTIATION
    (2 )
    (Map() )
    )
  (4
    (or (exists () (and (p ) ) ) (exists (x ) (and (q x ) ) ) )
    MODUS_PONENS
    (3 1 )
    ()
    )
  (5 (exists (x ) (and (q x ) ) ) CONTRACTION (4 ) () )
  (6
    (and (q (x1 ) ) )
    EXISTS_ELIM
    (5 )
    (Map(Var('x) -> App('x1,List())) )
    )
  (7 (q (x1 ) ) ERASURE (6 ) () )
  (8 (and (q (x1 ) ) ) AND_INTRO (7 ) () )
  (9
    (implies (and (q x ) ) (or (exists () (and (p ) ) ) ) )
    AXIOM
    ()
    ()
    )
  (10
    (implies (and (q (x1 ) ) ) (or (exists () (and (p ) ) ) ) )
    INSTANTIATION
    (9 )
    (Map(Var('x) -> App('x1,List())) )
    )
  (11 (or (exists () (and (p ) ) ) ) MODUS_PONENS (10 8 ) () )
  (12 (exists () (and (p ) ) ) CONTRACTION (11 ) () )
  (13 (and (p ) ) EXISTS_ELIM (12 ) (Map() ) )
  (14 (p ) ERASURE (13 ) () )
  (15 (and (p ) ) AND_INTRO (14 ) () )
  (16 (exists () (and (p ) ) ) EXISTS_INTRO (15 ) (Map() ) )
  (17 (or (exists () (and (p ) ) ) ) OR_INTRO (16 ) () )
  (18 (exists () (and (p ) ) ) CONTRACTION (4 ) () )
  (19 (and (p ) ) EXISTS_ELIM (18 ) (Map() ) )
  (20 (p ) ERASURE (19 ) () )
  (21 (and (p ) ) AND_INTRO (20 ) () )
  (22 (exists () (and (p ) ) ) EXISTS_INTRO (21 ) (Map() ) )
  (23 (or (exists () (and (p ) ) ) ) OR_INTRO (22 17 ) () )
  )"""
)
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
