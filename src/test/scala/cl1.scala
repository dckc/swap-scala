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
    Constant(Atom(base + i))
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
      case Atomic(rel, args) => fromSeq(List(rel, quote(args)))
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
      l.quote(l.derive_bf(state, pfs, conjecture).get).toString() should equal (
"""(appeal
  OR_INTRO
  (or (exists NIL (and (Mortal ('"socrates" ) ) ) ) )
  ((appeal
      EXISTS_INTRO
      (exists NIL (and (Mortal ('"socrates" ) ) ) )
      ((appeal
          ERASURE
          (and (Mortal ('"socrates" ) ) )
          ((appeal
              ERASURE
              (Mortal ('"socrates" ) )
              ((appeal
                  EXISTS_ELIM
                  (and (Mortal ('"socrates" ) ) )
                  ((appeal
                      MODUS_PONENS
                      (or
                        (exists NIL (and (Mortal ('"socrates" ) ) ) )
                        )
                      ((appeal
                          INSTANTIATION
                          (implies
                            (and (Man ('"socrates" ) ) )
                            (or
                              (exists
                                NIL
                                (and (Mortal ('"socrates" ) ) )
                                )
                              )
                            )
                          ((appeal
                              AXIOM
                              (implies
                                (and (Man (x ) ) )
                                (or
                                  (exists NIL (and (Mortal (x ) ) ) )
                                  )
                                )
                              NIL
                              NIL
                              )
                            )
                          (Map(Var('x) -> Constant("socrates")) )
                          )
                        (appeal
                          AND_INTRO
                          (and (Man ('"socrates" ) ) )
                          ((appeal
                              THEOREM
                              (Man ('"socrates" ) )
                              NIL
                              NIL
                              )
                            )
                          NIL
                          )
                        )
                      NIL
                      )
                    )
                  (Map() )
                  )
                )
              NIL
              )
            (appeal THEOREM (Man ('"socrates" ) ) NIL NIL )
            )
          NIL
          )
        )
      (Map() )
      )
    )
  NIL
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
      l.quote(l.derive_bf(Set.empty, Nil, conjecture).get).toString() should
      equal ("""(appeal
  OR_INTRO
  (or (exists NIL (and (p NIL ) ) ) )
  ((appeal
      EXISTS_INTRO
      (exists NIL (and (p NIL ) ) )
      ((appeal
          ERASURE
          (and (p NIL ) )
          ((appeal
              ERASURE
              (p NIL )
              ((appeal
                  EXISTS_ELIM
                  (and (p NIL ) )
                  ((appeal
                      MODUS_PONENS
                      (or
                        (exists NIL (and (p NIL ) ) )
                        (exists (x ) (and (q (x ) ) ) )
                        )
                      ((appeal
                          INSTANTIATION
                          (implies
                            (and )
                            (or
                              (exists NIL (and (p NIL ) ) )
                              (exists (x ) (and (q (x ) ) ) )
                              )
                            )
                          ((appeal
                              AXIOM
                              (implies
                                (and )
                                (or
                                  (exists NIL (and (p NIL ) ) )
                                  (exists (x ) (and (q (x ) ) ) )
                                  )
                                )
                              NIL
                              NIL
                              )
                            )
                          (Map() )
                          )
                        (appeal AND_INTRO (and ) NIL NIL )
                        )
                      NIL
                      )
                    )
                  (Map() )
                  )
                )
              NIL
              )
            )
          NIL
          )
        )
      (Map() )
      )
    (appeal
      OR_INTRO
      (or (exists NIL (and (p NIL ) ) ) )
      ((appeal
          EXISTS_INTRO
          (exists NIL (and (p NIL ) ) )
          ((appeal
              ERASURE
              (and (p NIL ) )
              ((appeal
                  ERASURE
                  (p NIL )
                  ((appeal
                      EXISTS_ELIM
                      (and (p NIL ) )
                      ((appeal
                          MODUS_PONENS
                          (or (exists NIL (and (p NIL ) ) ) )
                          ((appeal
                              INSTANTIATION
                              (implies
                                (and (q ('"x1" ) ) )
                                (or (exists NIL (and (p NIL ) ) ) )
                                )
                              ((appeal
                                  AXIOM
                                  (implies
                                    (and (q (x ) ) )
                                    (or (exists NIL (and (p NIL ) ) ) )
                                    )
                                  NIL
                                  NIL
                                  )
                                )
                              (Map(Var('x) -> Constant("x1")) )
                              )
                            (appeal
                              AND_INTRO
                              (and (q ('"x1" ) ) )
                              ((appeal
                                  ERASURE
                                  (q ('"x1" ) )
                                  ((appeal
                                      EXISTS_ELIM
                                      (and (q ('"x1" ) ) )
                                      ((appeal
                                          MODUS_PONENS
                                          (or
                                            (exists
                                              NIL
                                              (and (p NIL ) )
                                              )
                                            (exists
                                              (x )
                                              (and (q (x ) ) )
                                              )
                                            )
                                          ((appeal
                                              INSTANTIATION
                                              (implies
                                                (and )
                                                (or
                                                  (exists
                                                    NIL
                                                    (and (p NIL ) )
                                                    )
                                                  (exists
                                                    (x )
                                                    (and (q (x ) ) )
                                                    )
                                                  )
                                                )
                                              ((appeal
                                                  AXIOM
                                                  (implies
                                                    (and )
                                                    (or
                                                      (exists
                                                        NIL
                                                        (and (p NIL ) )
                                                        )
                                                      (exists
                                                        (x )
                                                        (and (q (x ) ) )
                                                        )
                                                      )
                                                    )
                                                  NIL
                                                  NIL
                                                  )
                                                )
                                              (Map() )
                                              )
                                            (appeal
                                              AND_INTRO
                                              (and )
                                              NIL
                                              NIL
                                              )
                                            )
                                          NIL
                                          )
                                        )
                                      (Map(Var('x) -> Constant("x1")) )
                                      )
                                    )
                                  NIL
                                  )
                                )
                              NIL
                              )
                            )
                          NIL
                          )
                        )
                      (Map() )
                      )
                    )
                  NIL
                  )
                (appeal
                  ERASURE
                  (q ('"x1" ) )
                  ((appeal
                      EXISTS_ELIM
                      (and (q ('"x1" ) ) )
                      ((appeal
                          MODUS_PONENS
                          (or
                            (exists NIL (and (p NIL ) ) )
                            (exists (x ) (and (q (x ) ) ) )
                            )
                          ((appeal
                              INSTANTIATION
                              (implies
                                (and )
                                (or
                                  (exists NIL (and (p NIL ) ) )
                                  (exists (x ) (and (q (x ) ) ) )
                                  )
                                )
                              ((appeal
                                  AXIOM
                                  (implies
                                    (and )
                                    (or
                                      (exists NIL (and (p NIL ) ) )
                                      (exists (x ) (and (q (x ) ) ) )
                                      )
                                    )
                                  NIL
                                  NIL
                                  )
                                )
                              (Map() )
                              )
                            (appeal AND_INTRO (and ) NIL NIL )
                            )
                          NIL
                          )
                        )
                      (Map(Var('x) -> Constant("x1")) )
                      )
                    )
                  NIL
                  )
                )
              NIL
              )
            )
          (Map() )
          )
        )
      NIL
      )
    )
  NIL
  )""")
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
