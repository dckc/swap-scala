package org.w3.swap.logic1c

import org.w3.swap
import swap.logic0.{Formula, Or, Not, Appeal}
import swap.logic1.{Equal, Term, FunctionTerm, Variable, FirstOrderLogic}
import swap.logic1.Term.Subst
import swap.sexp.{SExp, Atom, Cons}
import swap.sexp.SExp.{fromSeq}

/**
 * First order logic with equality, quoting, eval, and induction,
 * following ACL2/Milawa.
 */
abstract class ComputationalLogic extends FirstOrderLogic {

  def quote(f: Formula): SExp = {
    f match {
      // TODO: make a SExp constructor from List[Any]
      case Or(a, b) =>
	fromSeq(List(Symbol("POR*"), quote(a), quote(b)))

      case Not(a) =>
	fromSeq(List(Symbol("PNOT*"), quote(a)))

      case Equal(x: Quotable, y: Quotable) =>
	fromSeq(List(Symbol("PEQUAL*"), x.quote(), y.quote() ))

      case _ =>
	if (wff(f)) throw new Exception("quote or wff is broken")
	else throw new RuntimeException("quote called on non-wff: " + f)
    }
  }

  // TODO: consider Set rather than List for thms
  override def appeal_step_ok(x: Appeal, thms: List[Formula]): Boolean = {
    x match {
      case Appeal('AXIOM, f, Nil, Nil) =>
	wff(f) && axiom(f)

      case Appeal('THEOREM, f, Nil, Nil) =>
	wff(f) && thms.contains(f)

      // TODO: equality, induction, base eval, ...

      case Appeal(name, f, subproofs, Nil)
      if methods.contains(name) => 
	rule(name)(subproofs.map(_.conclusion), f)

      case _ => false
    }
  }
}

class History(atbl: Map[Symbol, Int], axioms: List[Formula])
extends ComputationalLogic {
  override def wff(f: Formula): Boolean = {
    terms(f).forall {
      case v: Var => true
      case ft: App => atbl(ft.fun) == ft.args.length
      case _ => false // not a quotable term
    }
  }

  override def axiom(f: Formula): Boolean = wff(f) && axioms.contains(f)
}


trait Quotable {
  /**
   * An symbolic expression for this term.
   */
  def quote(): SExp
}

case class Var(sym: Symbol) extends Variable with Quotable {
  override def quote() = Atom(sym)
}

case class Constant(value: SExp) extends FunctionTerm with Quotable {
  override def fun = value
  override def args = Nil
  override def subst(s: Subst) = this

  val hd = Atom('QUOTE)
  override def quote() = Cons(hd, value)
}

case class App(sym: Symbol, params: List[Term with Quotable])
extends FunctionTerm with Quotable {
  override def fun = sym
  override def args = params
  override def subst(s: Subst) = {
    // Hmm... couldn't quite get the types arranged
    // to avoid asInstanceOf here.
    App(sym, params.map(_.subst(s).asInstanceOf[Term with Quotable]))
  }

  override def quote() = fromSeq(Atom(sym) :: params.map(_.quote()))
}

// TODO: lambda
