package org.w3.swap.logic1eq

import org.w3.swap
import swap.logic0.{Formula, PropositionalCalculus, Or, Not}
import swap.logic1.{Term, Variable}

import scala.annotation.tailrec

/**
 * Quantifier-free first order logic with equality (and no other predicates).
 */
abstract class FOLeq extends PropositionalCalculus {
  import Term.Subst

  def terms(f: Formula): Seq[Term] = {
    f match {
      case Or(a, b) => terms(a) ++ terms(b)
      case Not(a) => terms(a)
      case Equal(x: Term, y: Term) => x.terms() ++ y.terms()
      case _ => Seq.empty // not wff
    }
  }

  def variables(f: Formula): Set[Variable] = {
    terms(f).partialMap { case v: Variable => v } toSet
  }

  /**
   * @return: if wff(f), the usual, else f
   * TODO: don't make a new formula unless we have to.
   */
  def subst(f: Formula, s: Subst): Formula = {
    f match {
      case Or(a, b) => Or(subst(a, s), subst(b, s))
      case Not(a) => Not(subst(a, s))
      case Equal(x: Term, y: Term) => Equal(x.subst(s), y.subst(s))
      case _ => f // not wff
    }
  }
}

case class Equal(x: Term, y: Term) extends Formula
