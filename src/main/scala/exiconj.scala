package org.w3.swap.logic1ec

import org.w3.swap
import swap.logic0.{Formula, PropositionalCalculus, Or, Not, Appeal}
import swap.logic1.{Term, Variable}

import scala.annotation.tailrec

// TODO: be sure we don't need arity for rel
case class Atomic(rel: Symbol, args: List[Term]) extends Formula
case class And(fmlas: Seq[Formula]) extends Formula
case class Exists(vars: Set[Variable], g: Formula) extends Formula

/**
 * Existential Conjunctive Logic
 */
class ECLogic extends PropositionalCalculus {
  import Term.Subst

  def terms(f: Formula): Seq[Term] = {
    f match {
      case Exists(vars, g) => vars.toList ++ terms(g)
      case And(fmlas) => fmlas.flatMap(terms _)
      case Atomic(rel, terms) => terms.flatMap(_.terms())
      case _ => Seq.empty // not wff
    }
  }

  def variables(f: Formula): Set[Variable] = {
    terms(f).partialMap { case v: Variable => v } toSet
  }

  def freevars(f: Formula): Set[Variable] = {
    f match {
      case Exists(vars, g) => freevars(g).filter(!vars.contains(_))
      case _ => variables(f)
    }
  }

  // scalaQ: how to constrain Exists subfmla to And? problems with subst()
  def wff(f: Formula): Boolean = {
    def wfatom(f: Formula): Boolean = f match {
      case a: Atomic => true
      case _ => false
    }

    def wfconj(f: Formula): Boolean = f match {
      case And(fmlas) => fmlas.forall(wfatom _)
      case _ => false
    }

    freevars(f).isEmpty && (f match {
      case Exists(vars, g) => wfconj(g)
      case _ => false
    })
  }

  def axiom(f: Formula) = false

  /**
   * @return: if wff(f), the usual, else f
   * TODO: don't make a new formula unless we have to.
   */
  def subst(f: Formula, s: Subst): Formula = {
    f match {
      /**
       * assume the vars in s don't occur in vars.
       * i.e. you can only substitute for free variables.
       * TODO: assert this. */
      case Exists(vars, g) => Exists(vars, subst(f, s))
      case And(fmlas) => And(fmlas.map(subst(_, s)))

      case Atomic(rel, terms) => Atomic(rel, terms.map(_.subst(s)))
      case _ => f // not wff
    }
  }

  // TODO
  def appeal_step_ok(x: Appeal, thms: List[Formula]): Boolean = false
}
