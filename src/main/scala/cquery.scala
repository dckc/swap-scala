package org.w3.swap.logic1ec

import scala.annotation.tailrec

import org.w3.swap
import swap.logic1.{Term, FunctionTerm, Variable}
import Term.{Subst, matchAll, mksubst}

/**
 * cf. <a href="http://en.wikipedia.org/wiki/Conjunctive_query"
 * >Conjunctive query in wikipedia</a>
 * 
 */
trait ConjunctiveQuery[A <: AtomicParts] {
  /**
   * Note: treats any variables in the facts as constants.
   */
  def solve(goals: Iterable[A], s: Subst,
	       facts: (() => Stream[A])): Stream[Subst] = {
    def solve1(goal: A): Stream[Subst] = {
      facts().flatMap { case fact =>
	if (fact.rel == goal.rel) {
	  matchAll(goal.args, fact.args, s).toStream
	} else Stream.empty }
    }

    if (goals.isEmpty) Stream.cons(s, Stream.empty) else {
      solve1(goals.head).flatMap(solve(goals.tail, _, facts))
    }
  }
}

trait AtomicParts {
  val rel: Symbol
  val args: List[Term]
}

abstract class ECProver extends ECLogic with ConjunctiveQuery[Atomic] {

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
	  val freshfn = scope(vf ++ vg)
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

  def atoms(f: ECFormula): List[Atomic] = {
    f match {
      case atom: Atomic => List(atom)
      case And(atoms) => atoms.toList
      case Exists(vars, And(atoms)) => atoms.toList
    }
  }

  def proofs(f: ECFormula, g: ECFormula): Stream[Subst] = {
    val facts = atoms(f)

    solve(atoms(g), Map(), { () => facts.toStream } )
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

  // kinda odd not to offer solving a collection of atoms together
  def solve(goal: Atomic): Stream[Subst] = {
    solve(List(goal), Map(), { () => getData(tokens(goal.args)) } )
  }
}
