package org.w3.swap.logic1ec

import org.w3.swap
import swap.logic0
import swap.logic0.{FormalSystem, Or, Not}
import swap.logic1.{Term, Variable}

import scala.annotation.tailrec

trait AtomicParts {
  val rel: Symbol
  val args: List[Term]
}


sealed abstract class ECFormula
case class Exists(vars: Set[Variable], g: And) extends ECFormula
sealed abstract class Ground extends ECFormula
// TODO: be sure we don't need arity for rel
case class And(fmlas: Seq[Atomic]) extends Ground
case class Atomic(rel: Symbol, args: List[Term]) extends Ground with AtomicParts

/**
 * Existential Conjunctive Logic
 */
class ECLogic extends FormalSystem {
  override type Formula = ECFormula
  import Term.Subst

  def freevars(f: Formula): Set[Variable] = {
    f match {
      case Exists(vars, g) => freevars(g).filter(!vars.contains(_))
      case And(fmlas) => fmlas.flatMap(freevars _) toSet
      case Atomic(rel, terms) =>
	terms.partialMap { case v: Variable => v } toSet
    }
  }

  def wff(f: Formula): Boolean = freevars(f).isEmpty

  def axiom(f: Formula) = false

  /**
   * TODO: don't make a new formula unless we have to.
   */
  def subst(f: Atomic, s: Subst): Atomic = {
    Atomic(f.rel, f.args.map(_.subst(s)))
  }
  def subst(f: And, s: Subst): And = {
    And(f.fmlas.map(subst(_, s)))
  }
  /**
   * assume the vars in s don't occur in vars.
   * i.e. you can only substitute for free variables.
   * TODO: assert this. */
  def subst(f: ECFormula, s: Subst): ECFormula = {
    f match {
      case Exists(vars, a) => Exists(vars, subst(a, s))
      case x: And => subst(x, s)
      case x: Atomic => subst(x, s)
    }
  }

  // TODO
  def appeal_step_ok(x: Appeal, thms: List[Formula]): Boolean = false

  override val methods = List[Symbol]()

  override def rule(method: Symbol): Rule = {     // TODO
    case (premises, conclusion) => false
  }
}
