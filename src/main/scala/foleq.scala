package org.w3.swap.logic1

import org.w3.swap
import swap.logic0.{PropositionalCalculus}
// TODO: migrate away from swap.logic
import swap.logic.{Formula, Atomic, Variable, FunctionTerm}
import swap.logic.Term // @@TODO: move this
import swap.sexp.SExp.fromSeq
import swap.logic.AbstractSyntax.Subst

case class Equal(x: Term, y: Term) extends Atomic {
  override def terms() = List(x, y)
  override def quote() = fromSeq(List('=, x.quote(), y.quote()))
  override def subst(s: Subst) = Equal(x.subst(s), y.subst(s))
}

/**
 * First order logic with equality, quoting, and induction,
 * following ACL2/Milawa.
 */
class ComputationalLogic(atbl: Map[Any, Int]) extends PropositionalCalculus {
  // TODO: atbls: Map[Symbol, Int]? SExp.Atom?
  //@@ TODO: type Formula = F0

  def wff(f: Formula): Boolean = {
    f.terms().forall {
      case v: Variable => true
      case ft: FunctionTerm => atbl(ft.fun) == ft.args.length
    }
  }

}
