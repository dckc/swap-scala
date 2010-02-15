package org.w3.swap.logic1cl

import org.w3.swap
import swap.logic0.{FormalSystem}
import swap.logic1.{Term, Variable, FunctionTerm}
import Term.{Subst, matchAll, mksubst}
import swap.logic1ec.{AtomicParts, ConjunctiveQuery}

/**
 * Coherent Logic
 * see Automating Coherent Logic
 * Marc Bezem1 and Thierry Coquand2
 * in http://folli.loria.fr/cds/2006/courses/Bezem.Nivelle.IntroductionToAutomatedReasoning.pdf
 * starting on pg 80
 *
 * See #swig logs
 * http://chatlogs.planetrdf.com/swig/2010-02-12#T23-17-03
 *
 * TODO: split out proof generation part
 */

abstract class CoherentLogic(theory: Seq[Implication])
extends FormalSystem with ConjunctiveQuery[Atomic]{
  type Formula = CLFormula

  def axiom(f: Formula) = theory contains f

  /**
   * Does the conclusion consist of a single Atom?
   */
  def horn(f: Formula): Boolean = {
    f match {
      /* this is only split for line length. hmm. */
      case Implication(_, Disjunction(List(e1))) => e1 match {
	case Exists(xi, Conjunction(List(atom))) => xi.isEmpty
	case _ => false
      }
      case _ => false
    }
  }

  def wff(f: Formula) = {
    f match {
      case i: Implication => true
      case _ => freevars(f).isEmpty
    }
  }

  def freevars(f: CLFormula): Set[Variable] = {
    f match {
      case Implication(c, d) => freevars(c) ++ freevars(d)
      case Disjunction(ei) => ei.toSet.flatMap{ e: Exists => freevars(e) }
      case Exists(xi, c) => freevars(c) &~ xi
      case Conjunction(ai) => ai.toSet.flatMap{ a: Atomic => freevars(a) }
      case Atomic(rel, args) => args.toSet.flatMap {
	arg: Term => arg.variables() }
    }
  }

  val bottom = Disjunction(Nil)

  type State = Set[Atomic] // all closed

  def true_in(c: Conjunction, state: State): Boolean = {
    assert(state.forall(wff _))
    assert(wff(c))
    c.ai.forall(state.contains(_))
  }

  def true_in(d: Disjunction, state: State): Boolean = {
    assert(state.forall(wff _))
    assert(wff(d))
    d.ei.exists { case Exists(xi, c) => !solve(c, state).isEmpty }
  }

  def fresh(pattern: Variable): Variable
  def parameter(pattern: Variable): FunctionTerm

  def subst(c: Conjunction, s: Subst): Conjunction = {
    Conjunction(c.ai.map {
      a => Atomic(a.rel, a.args.map(_.subst(s)))
    })
  }

  def subst(d: Disjunction, s: Subst): Disjunction = {
    Disjunction(d.ei.map {
      e => Exists(e.xi, subst(e.c, s))
    })
  }

  def closed_instance(c: Conjunction): Conjunction = {
    val (s, vars) = mksubst(freevars(c), Nil, parameter, Map())
    subst(c, s)
  }


  /**
   * all possible combinations of selecting one disjunct from each Di
   */
   def combinations(chosen: Set[Conjunction],
     todo: List[Disjunction]): Stream[Set[Conjunction]] = {
     todo match {
       case Nil => Stream(chosen)
       case di :: dn => {
	 di.ei.toStream.flatMap { case Exists(_, cij) =>
	   combinations(chosen + closed_instance(cij), dn) }
       }
     }
   }

  /**
   * @return true if d is a breadth-first consequence of x in t
   * Loops forever if not, as ECLogic is only semi-decidable. :-/
   */
  def consequence_bf(x: State, d: Disjunction): Boolean = {
    assert(x.forall(wff _))
    assert(wff(d))

    println("@@search conjecture: " + d)

    def search(x: State): Boolean = {
      println("@@search state: " + x)

      if (true_in(d, x)) true // base case
      else { // induction step
	val d0n = for {
	  Implication(c, d) <- theory
	  solution <- solve(c, x)
	  consequently = assert(true_in(subst(c, solution), x))
	  di = subst(d, solution)
	  if !true_in(di, x)
	} yield di

	combinations(Set.empty, d0n.toList).forall {
	  cset: Set[Conjunction] => search(x union cset.flatMap(_.ai.toSet)) }
      }
    }

    search(x)
  }

  def solve(c: Conjunction, state: Set[Atomic]): Stream[Subst] = {
    solve(c.ai, Map(), {() => state.toStream})
  }

  
  // TODO
  def appeal_step_ok(x: Appeal, thms: List[Formula]): Boolean = false
  override val methods = List[Symbol]()

  override def rule(method: Symbol): Rule = {     // TODO
    case (premises, conclusion) => false
  }
}

/**
 * Definition 1: Coherent formula, disjunction, conjunction, implication.
 */
sealed abstract class CLFormula
case class Implication(c: Conjunction, d: Disjunction) extends CLFormula
case class Conjunction(ai: List[Atomic]) extends CLFormula
case class Atomic(rel: Symbol, args: List[Term]) extends CLFormula
   with AtomicParts
case class Disjunction(ei: List[Exists]) extends CLFormula
case class Exists(xi: Set[Variable], c: Conjunction) extends CLFormula
