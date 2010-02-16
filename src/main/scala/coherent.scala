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
  type Formula = Implication

  def axiom(f: Formula) = theory contains f

  /**
   * Does the conclusion consist of a single Atom?
   */
  def horn(f: Formula): Boolean = f.q.ei match {
    case List(e1) => e1.xi.isEmpty && e1.c.ai.length == 1
    case _ => false
  }

  def wff(f: Formula) = {
    f match {
      case c: Conjunction => freevars(f).isEmpty
      case d: Disjunction => freevars(f).isEmpty
      case _ => true
    }
  }

  def freevars(f: Formula): Set[Variable] = {
    f match {
      case Implies(c, d) => freevars(c) ++ freevars(d)
      case Or(ei) => ei.toSet.flatMap{ e: Existential => freevars(e) }
      case Exists(xi, c) => freevars(c) &~ xi
      case And(ai) => ai.toSet.flatMap{ a: Atomic => freevars(a) }
      case Atomic(rel, args) => args.toSet.flatMap {
	arg: Term => arg.variables() }
    }
  }

  val bottom = Or(Nil)

  def derive1(c: Conjunction, pfs: List[Appeal]): Option[Appeal] = {
    assert(wff(c))
    val state = pfs.map { case Appeal(_, a: Atomic, _, _) => a }
    assert(state.forall(wff _))

    val goalqty = c.ai.length
    val relevant = c.ai.flatMap {a => pfs.find(_.conclusion == a)}
    if (relevant.length == goalqty) {
      Some(if (goalqty == 1) pfs(0)
	   else Appeal('AND_INTRO, c, relevant, Nil) )
    } else None
  }

  def derive1(d: Disjunction, pfs: List[Appeal]): Option[Appeal] = {
    assert(wff(d))
    val state = pfs.map { case Appeal(_, a: Atomic, _, _) => a }
    assert(state.forall(wff _))

    val answers = for {
      e <- d.ei.toStream
      s <- solve(e.c, state)
      c_closed = subst(e.c, s)
    } yield {
      // pfs.find is perhaps re-doing work that was done in solve?
      val relevant = c_closed.ai.map {
	a => pfs.find(_.conclusion == a).get
      }
      val pfc = {
	if (relevant.length == 1) relevant(0)
	else Appeal('AND_INTRO, subst(e.c, s), relevant, Nil)
      }
      if (s.isEmpty) pfc
      else Appeal('EXISTS_INTRO, e, List(pfc), List(s))
    }
    if (answers.isEmpty) None else Some(answers.head)
  }

  def fresh(pattern: Variable): Variable
  def parameter(pattern: Variable): FunctionTerm

  def substa(a: Atomic, s: Subst) = Atomic(a.rel, a.args.map(_.subst(s)))

  def subst(c: Conjunction, s: Subst): Conjunction = {
    Conjunction(c.ai.map(substa(_, s)))
  }

  def subst(d: Disjunction, s: Subst): Disjunction = {
    Disjunction(d.ei.map {
      case Atomic(rel, terms) => Atomic(rel, terms.map(_.subst(s)))
      case And(ai) => And(ai.map(substa(_, s)))
      case e => Exists(e.xi, subst(e.c, s))
    })
  }

  def closed_instance(c: Conjunction): Conjunction = {
    val (s, params) = mksubst(freevars(c), Nil, parameter, Map())
    subst(c, s)
  }


  /**
   * all possible combinations of selecting one disjunct from each Di
   */
   def combinations(chosen: List[Conjunction],
		    todo: List[Disjunction]): Stream[List[Conjunction]] = {
     todo match {
       case Nil => Stream(chosen)
       case d0 :: d1n => {
	 d0.ei.toStream.flatMap {
	   e => combinations(closed_instance(e.c) :: chosen, d1n)
	 }
       }
     }
   }

  /**
   * @return true if X |- D in theory
   * Loops forever if not, as ECLogic is only semi-decidable. :-/
   */
  def consequence(state: List[Appeal], d: Disjunction): Option[Appeal] = {
    assert(state.forall { a => wff(a.conclusion) } )
    assert(wff(d))

    println("@@consequence conjecture: " + d)

    def checkstate(state: List[Appeal]): Option[Appeal] = {
      println("@@checkstate facts: " + state)

      val base = derive1(d, state)
      if (!base.isEmpty) base
      else { // induction step
	val d_pf0n = nontrivial_conclusions(state)
	if (d_pf0n.isEmpty) None
	else {
	  val (d0, pf0) = d_pf0n.head
	  val branches = d0.ei.length
	  if (branches == 0) None
	  else if (branches == 1) Some(pf0)
	  else {
	    def ck = d0.ei.map { e => checkstate(branch_facts(e) ++ state) }
	    if (ck.forall(!_.isEmpty)) {
	      Some(Appeal('OR_INTRO, d, pf0 :: ck.map(_.get), Nil))
	    } else None
	  }
	}
      }
    }

    checkstate(state)
  }

  def branch_facts(e: Existential): List[Appeal] = {
    val pfe = Appeal('ASSUMPTION, e, Nil, Nil)
    val (c, pfc) = {
      if (e.xi.isEmpty) (e.c, pfe)
      else {
	val (s, params) = mksubst(e.xi, Nil, parameter, Map())
	val cij_closed = subst(e.c, s)
	(cij_closed,
	 Appeal('EXISTS_ELIM, cij_closed, List(pfe), List(s)) )
      }
    }

    // TODO: maybe Appeal[T](...)
    if (c.ai.length == 1) List(pfc)
    else c.ai.map(Appeal('ERASURE, _, List(pfc), Nil))
  }


  def nontrivial_conclusions(pfs: List[Appeal]): Stream[(Disjunction,
							 Appeal)] = {
    val x = pfs.map { case Appeal(_, a: Atomic, _, _) => a }

    for {
      ax <- theory.toStream
      c = ax.p
      d = ax.q
      solution <- solve(c, x)
      ci = subst(c, solution)
      di = subst(d, solution)
      if derive1(di, pfs).isEmpty
      imp_closed = Implication(ci, di)
    } yield {
      val pf_ax = Appeal('AXIOM, ax, Nil, Nil)

      val pf_imp = {
	if (imp_closed == ax) pf_ax
	else Appeal('INSTANTIATION, imp_closed, List(pf_ax), List(solution))
      }

      val pf_di = {
	if (di == imp_closed) pf_imp
	else {
	  val pf_ci = derive1(ci, pfs).get
	  Appeal('MODUS_PONENS, di, List(pf_imp, pf_ci), Nil)
	}
      }
      
      (di, pf_di)
    }
  }

  def solve(c: Conjunction, state: List[Atomic]): Stream[Subst] = {
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

object Implication {
  def apply(p: Conjunction, q: Disjunction): Implication = {
    if (p.ai.isEmpty) q
    else Implies(p, q)
  }
}

sealed abstract class Implication {
  def p: Conjunction
  def q: Disjunction
}
case class Implies(val c: Conjunction,
		   val d: Disjunction) extends Implication {
  // use q rather than And() -> q
  require (!p.ai.isEmpty)

  // odd... doing this to avoid loop/stack overflow
  def p = c
  def q = d
}

object Disjunction {
  def apply(ei: List[Existential]): Disjunction = {
    if (ei.length == 1) ei(0)
    else Or(ei)
  }
}

sealed abstract class Disjunction extends Implication {
  def p = And(Nil)
  def q = this
  def ei: List[Existential]
}
case class Or(val fmlas: List[Existential]) extends Disjunction {
  // use f rather than Or(f)
  require (ei.length != 1)
  def ei = fmlas
}

object Existential {
  def apply(xi: Set[Variable], c: Conjunction): Existential = {
    if (xi.isEmpty) c
    else Exists(xi, c)
  }
}

sealed abstract class Existential extends Disjunction {
  override def ei = List(this)
  def xi: Set[Variable]
  def c: Conjunction
}
case class Exists(val xi0: Set[Variable],
		  val c0: Conjunction) extends Existential {
  // use f rather than Exists(empty, f)
  require (!xi.isEmpty)
  def xi = xi0
  def c = c0
}

object Conjunction {
  def apply(ai: List[Atomic]): Conjunction = {
    if (ai.length == 1) ai(0)
    else And(ai)
  }
}
abstract class Conjunction extends Existential {
  override def xi: Set[Variable] = Set.empty
  override def c = this
  def ai: List[Atomic]
}
case class And(val ai0: List[Atomic]) extends Conjunction{
  // use f rather than And(f)
  require (ai.length != 1)
  override def ai = ai0
}

case class Atomic(rel: Symbol, args: List[Term]) extends Conjunction
   with AtomicParts{
  override def ai = List(this)
}
