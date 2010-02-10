package org.w3.swap.logic1

import org.w3.swap
import swap.logic0.{Formula, PropositionalCalculus, Or, Not}

import scala.annotation.tailrec

/**
 * Quantifier-free first order logic with equality.
 */
abstract class FirstOrderLogic extends PropositionalCalculus {
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

/**
 * A Term is either a Variable or an FunctionTerm.
 */
sealed abstract class Term {
  import Term.Subst

  /**
   * This term plus any subterms.
   */
  def terms(): Seq[Term]

  /**
   * All variables occurring in this term.
   */
  def variables(): Set[Variable]

  /**
   * This term with the variables in s replaced by their values.
   * TODO: document and test this better.
   */
  def subst(s: Subst): Term
}

// try to preserve order to facilitate testing
import scala.collection.immutable.ListSet

class Variable extends Term {
  override def terms() = Seq(this)
  override def variables() = ListSet(this)

  override def subst(s: Term.Subst) = Term.lookup(this, s)
}

abstract class FunctionTerm() extends Term {
  def fun: Any
  def args: List[Term]

  override def terms() = Seq(this) ++ args.flatMap(_.terms())
  override def variables() = {
    ListSet.empty ++ args.flatMap(_.variables())
  }
}

object Term {
  type Subst = Map[Variable, Term]

  /**
   * Deep/recursive lookup.
   * i.e. if x is bound to y and y is bound to 1, then lookup(x) => 1.
   * TODO: doctest this.
   */
  def lookup(t: Term, s:Subst): Term = {
    t match {
      case v: Variable => {
	s.get(v) match {
	  case None => t
	  case Some(b) => lookup(b, s)
	}
      }
      case _ => t
    }
  }

  def unify(tt1: Term, tt2: Term, s: Subst): Option[Subst] = {
    val t1 = lookup(tt1, s)
    val t2 = lookup(tt2, s)
    if (t1 == t2) Some(s)
    else t1 match {
      case v1: Variable => Some(s + (v1 -> t2))
      case a1: FunctionTerm => t2 match {
	case v2: Variable => Some(s + (v2 -> t1))
	case a2: FunctionTerm => {
	  if (a1.fun == a2.fun) unifyall(a1.args, a2.args, s)
	  else None
	}
      }
    }
  }

  @tailrec
  def unifyall(ts1: Seq[Term], ts2: Seq[Term], s: Subst): Option[Subst] = {
    (ts1.isEmpty, ts2.isEmpty) match {
      case (true, true) => Some(s)
      case (true, false) => None
      case (false, true) => None
      case _ => {
	unify(ts1.head, ts2.head, s) match {
	  case None => None
	  case Some(ss) => unifyall(ts1.tail, ts2.tail, ss)
	}
      }
    }
  }

  def matchTerm(pattern: Term, data: Term, s: Subst): Option[Subst] = {
    val pat = lookup(pattern, s)
    if (pat == data) Some(s)
    else pat match {
      case v1: Variable => Some(s + (v1 -> data))
      case a1: FunctionTerm => data match {
	case v2: Variable => None
	case a2: FunctionTerm => {
	  if (a1.fun == a2.fun) matchAll(a1.args, a2.args, s)
	  else None
	}
      }
    }
  }

  @tailrec
  def matchAll(pats: Seq[Term], data: Seq[Term], s: Subst): Option[Subst] = {
    (pats.isEmpty, data.isEmpty) match {
      case (true, true) => Some(s)
      case (true, false) => None
      case (false, true) => None
      case _ => {
	matchTerm(pats.head, data.head, s) match {
	  case None => None
	  case Some(ss) => matchAll(pats.tail, data.tail, ss)
	}
      }
    }
  }
}

case class Equal(x: Term, y: Term) extends Formula
