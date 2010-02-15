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

abstract class CoherentLogic(theory: List[Implication])
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

  def true_in(c: Conjunction, state: State,
	      pfs: List[Appeal]): Option[Appeal] = {
    assert(state.forall(wff _))
    assert(wff(c))
    if (c.ai.forall(state.contains(_))) {
      Some(Appeal('AND_INTRO, c,
		  c.ai.map {a => pfs.find(_.conclusion == a).get},
		  Nil))
    }
    else None
  }

  def true_in(d: Disjunction, state: State,
	      pfs: List[Appeal]): Option[Appeal] = {
    assert(state.forall(wff _))
    assert(wff(d))

    val answers = d.ei.toStream flatMap {
      e => {
	val solutions = solve(e.c, state)
	if (solutions.isEmpty) None
	else {
	  val s = solutions.head
	  val c_closed = subst(e.c, s)
	  // pfs.find is perhaps re-doing work that was done in solve?
	  val relevant = c_closed.ai.map {
	    a => pfs.find(_.conclusion == a).get }
	  val pfc = Appeal('AND_INTRO, subst(e.c, s), relevant, Nil)
	  Some(Appeal('EXISTS_INTRO, e, List(pfc), List(s)))
	}
      }
    }
    if (answers.isEmpty) None else Some(answers.head)
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
    val (s, params) = mksubst(freevars(c), Nil, parameter, Map())
    subst(c, s)
  }


  /**
   * all possible combinations of selecting one disjunct from each Di
   */
   def combinations(chosen: Set[Conjunction], pfs: List[Appeal],
     todo: List[(Disjunction, Appeal)]): Stream[(Set[Conjunction],
						 List[Appeal])] = {
     todo match {
       case Nil => Stream((chosen, pfs))
       case (d0, pf0) :: dp1n => {
	 d0.ei.toStream.flatMap {
	   case e @ Exists(xi, cij) => {
	     val (s, params) = mksubst(xi, Nil, parameter, Map())
	     val cij_closed = subst(cij, s)
	     val pfe = Appeal('CONTRACTION, e, List(pf0), Nil)
	     val pfc = Appeal('EXISTS_ELIM, cij_closed, List(pfe), List(s))
	     combinations(chosen + cij_closed, pfc :: pfs, dp1n)
	   }
	 }
       }
     }
   }

  /**
   * @return true if d is a breadth-first consequence of x in t
   * Loops forever if not, as ECLogic is only semi-decidable. :-/
   */
  def derive_bf(x: State, pfs: List[Appeal], d: Disjunction): Option[Appeal] = {
    assert(x.forall(wff _))
    assert(wff(d))

    println("@@search conjecture: " + d)

    def search(x: State, pfs: List[Appeal]): Option[Appeal] = {
      println("@@search state: " + x)

      val base = true_in(d, x, pfs)
      if (!base.isEmpty) base
      else { // induction step
	val d_pf0n = for {
	  ax <- theory
	  c = ax.c
	  d = ax.d
	  solution <- solve(c, x)
	  ci = subst(c, solution)
	  di = subst(d, solution)
	  if true_in(di, x, pfs).isEmpty
	  imp_closed = Implication(ci, di)
	} yield {
	  val pf_ci = true_in(ci, x, pfs).get

	  val pf_imp = Appeal('INSTANTIATION, imp_closed,
			      List(Appeal('AXIOM, ax, Nil, Nil)),
			      List(solution))

	  val pf_di = Appeal('MODUS_PONENS, di,
			     List(pf_imp, pf_ci), Nil)
	  (di, pf_di)
	}

	val trypfs = combinations(Set.empty, Nil, d_pf0n).map {
	  case (cset, cpfs) => {
	    val newfacts = cset.toList.flatMap(_.ai).toSet
	    val newpfs = cpfs.flatMap {
	      case pf @ Appeal(_, Conjunction(ai), _, _) => ai.map {
		fact => Appeal('ERASURE, fact, List(pf), Nil) }
	      case _ => Nil // can't actually happen
	    }
	    search(x union newfacts, newpfs ++ pfs)
	  }
	}

	// if the search above terminates, all cases were proved.
	assert(trypfs.forall(!_.isEmpty))
	val allpfs = trypfs.toList.map(_.get)
	Some(Appeal('OR_INTRO, d, allpfs, Nil))
      }
    }

    search(x, pfs)
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
 * TODO: allow Atomic where Conjunction goes, etc.
 */
sealed abstract class CLFormula
case class Implication(c: Conjunction, d: Disjunction) extends CLFormula
case class Conjunction(ai: List[Atomic]) extends CLFormula
case class Atomic(rel: Symbol, args: List[Term]) extends CLFormula
   with AtomicParts
case class Disjunction(ei: List[Exists]) extends CLFormula
case class Exists(xi: Set[Variable], c: Conjunction) extends CLFormula
