package org.w3.swap.logic1cl

import org.w3.swap
import swap.logic0.{FormalSystem}
import swap.logic1.{Term, Variable}

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
  def horn(f: CLFormula): Boolean = {
    f match {
      /* this is only split for line length. hmm. */
      case Implication(_, Disjunction(List(e1))) => e1 match {
	case Exists(xi, Conjunction(List(atom))) => xi.isEmpty
	case _ => false
      }
      case _ => false
    }
  }

  def terms(f: CLFormula): Set[Variable] //TODO
  def variables(f: CLFormula): Set[Variable] //TODO
  def freevars(f: CLFormula): Set[Variable] //TODO
  def closed(f: CLFormula) = freevars(f).isEmpty
  val bottom = Disjunction(Nil)

  def trueIn(c: Conjunction, state: Set[Atomic]): Boolean = {
    assert(state.forall(closed _))
    assert(closed(c))
    c.ai.forall(state.contains(_))
  }

  def trueIn(d: Disjunction, state: Set[Atomic]): Boolean = {
    assert(state.forall(closed _))
    assert(closed(d))
    d.ei.exists { case Exists(xi, c) => !solve(c, xi, state).isEmpty }
  }

  type Subst = Map[Variable, Term]
  def solve(c: Conjunction, xi: Set[Variable],
	    state: Set[Atomic]): Stream[Subst] // TODO
}

/**
 * Definition 1: Coherent formula, disjunction, conjunction, implication.
 */
sealed abstract class CLFormula
case class Atomic(rel: Symbol, args: List[Term]) extends CLFormula
case class Implication(c: Conjunction, d: Disjunction) extends CLFormula
case class Conjunction(ai: List[Atomic]) extends CLFormula
case class Disjunction(ei: List[Exists]) extends CLFormula
case class Exists(xi: Set[Variable], c: Conjunction) extends CLFormula

