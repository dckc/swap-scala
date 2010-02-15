package org.w3.swap.logic1cl

import org.w3.swap
import swap.logic0.{FormalSystem}
import swap.logic1.{Term, Variable}
import Term.{Subst, matchAll, mksubst}

/**
 * Coherent Logic
 * see Automating Coherent Logic
 * Marc Bezem1 and Thierry Coquand2
 * in http://folli.loria.fr/cds/2006/courses/Bezem.Nivelle.IntroductionToAutomatedReasoning.pdf
 * starting on pg 80
 *
 * See #swig logs
 * http://chatlogs.planetrdf.com/swig/2010-02-12#T23-17-03
 */
abstract class CoherentLogic extends FormalSystem {
  type Formula = CLFormula

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

  def closed(f: Formula) = freevars(f).isEmpty
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
  type Theory = Seq[Implication] // hmmm... Set? Iterable?

  type State = Set[Atomic] // all closed

  def true_in(c: Conjunction, state: State): Boolean = {
    assert(state.forall(closed _))
    assert(closed(c))
    c.ai.forall(state.contains(_))
  }

  def true_in(d: Disjunction, state: State): Boolean = {
    assert(state.forall(closed _))
    assert(closed(d))
    d.ei.exists { case Exists(xi, c) => !solve(c, xi, state).isEmpty }
  }

  def fresh(pattern: Variable): Variable

  def subst(c: Conjunction, s: Subst): Conjunction // TODO
  def subst(d: Disjunction, s: Subst): Disjunction // TODO

  def closed_instance(c: Conjunction): Conjunction = {
    val (s, vars) = mksubst(freevars(c), Nil, fresh _, Map())
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
  def consequence_bf(t: Theory, x: State, d: Disjunction): Boolean = {
    assert(x.forall(closed _))
    assert(closed(d))

    def search(x: State): Boolean = {
      if (true_in(d, x)) true // base case
      else { // induction step
	val d0n = for {
	  Implication(c, d) <- t
	  solution <- solve(c, freevars(c), x)
	  consequently = assert(true_in(c, x))
	  di = subst(d, solution)
	  if !true_in(di, x)
	} yield di

	combinations(Set.empty, d0n.toList).forall {
	  cset: Set[Conjunction] => search(x union cset.flatMap(_.ai.toSet)) }
      }
    }

    search(x)
  }

  // TODO: factor out Conjunctive Query as a trait
  type Subst = Map[Variable, Term]
  def solve(c: Conjunction, xi: Set[Variable],
	    state: Set[Atomic]): Stream[Subst] // TODO
}

/**
 * Definition 1: Coherent formula, disjunction, conjunction, implication.
 */
sealed abstract class CLFormula
case class Implication(c: Conjunction, d: Disjunction) extends CLFormula
case class Conjunction(ai: List[Atomic]) extends CLFormula
case class Atomic(rel: Symbol, args: List[Term]) extends CLFormula
case class Disjunction(ei: List[Exists]) extends CLFormula
case class Exists(xi: Set[Variable], c: Conjunction) extends CLFormula
