package org.w3.swap.logic1ec

import scala.annotation.tailrec

import org.w3.swap
import swap.logic1.{Term, FunctionTerm, Variable}
import Term.{Subst, matchAll}

/**
 * cf. <a href="http://en.wikipedia.org/wiki/Conjunctive_query"
 * >Conjunctive query in wikipedia</a>
 * 
 */

abstract class ECProver extends ECLogic {

  // odd... can't @tailrec
  def mksubst(todo: Iterable[Variable], done: List[Variable],
	      fresh: (Variable) => Variable,
	      s: Subst): (Subst, List[Variable]) = {
    if (todo.isEmpty) (s, done) else {
      val vr = fresh(todo.head)
      mksubst(todo.tail, vr :: done, fresh, s + (todo.head -> vr))
    }
  }

  /**
   * Given a set of variables that are taken,
   * return a function that makes fresh variables, patterned
   * on existing variables.
   */
  def scope(taken: Iterable[Variable]): (Variable => Variable)

  /**
   * preserved well-formedness
   * renames variables when necessary
   */
  def conjunction(f: ECFormula, g: ECFormula): ECFormula = {
    assert(wff(f))
    assert(wff(g))

    def mkand(f: Ground, g: Ground): And = {
      (f, g) match {
	case (And(fl), And(gl)) => And(fl ++ gl)
	case (x: Atomic, And(gl)) => And(Seq(x) ++ gl)
	case (And(fl), x: Atomic) => And(fl ++ Seq(x))
	case (x: Atomic, y: Atomic) => And(Seq(x, y))
      }
    }

    (f, g) match {
      case (Exists(vf, ff), Exists(vg, gg)) => {
	val shared = vf filter (vg contains)
	val (vg2, g2) = if (shared.isEmpty) (vg, gg) else {
	  val freshfn = scope(variables(f) ++ variables(g))
	  val (sub, freshvars) = mksubst(shared, Nil, freshfn, Map())
	  val g3 = subst(gg, sub)
	  (Set() ++ freshvars ++ (vg filterNot (shared contains)), g3)
	}

	Exists(vf ++ vg2, mkand(ff, g2))
      }
      case (Exists(vf, ff), y: Ground) => {
	Exists(vf, mkand(ff, y))
      }
      case (x: Ground, Exists(vg, gg)) => {
	Exists(vg, mkand(x, gg))
      }
      case (x: Ground, y: Ground) => mkand(x, y)
    }
  }

  def entails(f: ECFormula, g: ECFormula): Boolean = {
    ! proofs(f, g).isEmpty
  }

  def proofs(f: ECFormula, g: ECFormula): Stream[Subst] = {
    val facts = f match {
      case atom: Atomic => List(atom)
      case And(atoms) => atoms
      case Exists(vars, And(atoms)) => atoms
    }

    g match {
      case Exists(vars, gg) => solve(gg, Map(), { () => facts.toStream } )
      case x: Ground => solve(x, Map(), { () => facts.toStream })
    }
  }

  /**
   * Note: treats variables in the goal as constants.
   */
  def solve(goal: Ground, s: Subst,
	    facts: (() => Stream[Atomic])): Stream[Subst] = {
    goal match {
      case goalatom: Atomic => {
	facts().flatMap { case fact =>
	  if (fact.rel == goalatom.rel) {
	    matchAll(terms(goalatom), terms(fact), s).toStream
	  } else Stream.empty }
      }

      case And(fmlas) => solveall(fmlas, s, facts)
    }
  }

  def solveall(goals: Iterable[Atomic], s: Subst,
	       facts: (() => Stream[Atomic])): Stream[Subst] = {
    if (goals.isEmpty) Stream.cons(s, Stream.empty) else {
      solve(goals.head, s, facts).flatMap(solveall(goals.tail, _, facts))
    }
  }

}

abstract class ConjunctiveKB extends ECProver {
  /**
   * Given a set of function symbols,
   * return the atomic formulas that use those function symbols.
   */
  def getData(tokens: Seq[Any]): Stream[Atomic]

  def tokens(terms: Seq[Term]): Seq[Any] = {
    terms.flatMap {case ap: FunctionTerm => Some(ap.fun); case _ => None}
  }

  def solve(goal: Atomic): Stream[Subst] = {
    solve(goal, Map(), { () => getData(tokens(goal.args)) } )
  }
}
