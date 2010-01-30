package org.w3.swap.logic

import org.w3.swap
import swap.sexp.{SExp, Atom, Cons}
import swap.sexp.SExp.fromSeq

import scala.annotation.tailrec
// try to preserve order for testing
import scala.collection.immutable.ListSet

/**
 * Formula of an arbitrary first order logic.
 *
 * See <a href="http://en.wikipedia.org/wiki/First-order_logic#Formulas"
 * >Formulas</a> in the wikipedia article.
 *
 * A Formula is either an Atom, Unary, Binary, or Nary.
 *
 * TODO: generalize wellFormed from RDF to here.
 * TODO: add Not, Or, and Implies, and consider intuitionistic vs. classical.
 */
sealed abstract class Formula() {
  import AbstractSyntax.Subst

  def terms(): Seq[Term]
  def variables(): Set[Variable]
  def freevars(): Set[Variable]

  /* TODO: don't make a new formula unless we have to. */
  def subst(s: Subst): Formula

  def quote(): SExp

  override def toString() = quote().print()
}

abstract class Atomic extends Formula {
  override def variables(): Set[Variable] = {
    terms().foldLeft(ListSet.empty[Variable]) {
      (done, next) => done ++ next.variables()}
  }
  override def freevars() = variables()
}

abstract class Unary(f: Formula) extends Formula
abstract class Binary(f: Formula, g: Formula) extends Formula
abstract class Nary(val connective: Symbol,
		    val fl: Seq[Formula]) extends Formula {
  override def quote() = Cons(connective,
			      fromSeq(fl.toList.map(f => f.quote())) )

  override def terms() = fl.flatMap(fmla => fmla.terms())
  override def variables() = (ListSet.empty[Variable] ++
			      fl.flatMap(fmla => fmla.variables()) )
  override def freevars() = variables()
}

case class And(val fmlas: Seq[Formula]) extends Nary('and, fmlas){
  override def subst(s: AbstractSyntax.Subst) = {
    And(fmlas.map(fmla => fmla.subst(s)))
  }
}

sealed abstract class Quantified extends Formula {
  val vars: Set[Variable]
  val f: Formula

  protected val head: Symbol

  def terms() = List() ++ vars ++ f.terms() // scalaQ: why doesn't toSeq work?
  def variables() = vars ++ f.variables()
  def freevars() = f.freevars().filter(fv => !vars.contains(fv))

  def quote() = fromSeq(List(head,
			     fromSeq(vars.toSeq.map(v => v.quote())),
			     f.quote() ))
}

case class Exists(override val vars: Set[Variable],
		  override val f: Formula) extends Quantified {
  override val head = 'exists

  /**
   * assume the vars in s don't occur in vars.
   * i.e. you can only substitute for free variables.
   * TODO: assert this. */
  def subst(s: AbstractSyntax.Subst) = Exists(vars, f.subst(s))
}

case class Forall(override val vars: Set[Variable],
		  override val f: Formula) extends Quantified {
  override val head = 'forall

  /**
   * assume the vars in s don't occur in vars.
   * i.e. you can only substitute for free variables.
   * TODO: assert this. */
  def subst(s: AbstractSyntax.Subst) = Forall(vars, f.subst(s))
}


/**
 * Formula of an arbitrary first order logic.
 *
 * See <a href="http://en.wikipedia.org/wiki/First-order_logic#Terms"
 * >Terms</a> in the wikipedia article.
 *
 * A Term is either a Variable or an Application, i.e. a function term.
 */
sealed abstract class Term() {
  import AbstractSyntax.Subst

  /**
   * All variables occurring in this term.
   */
  def variables(): Set[Variable]

  /**
   * This term with the variables in s replaced by their values.
   * TODO: document and test this better.
   */
  def subst(s: Subst): Term

  /**
   * An symbolic expression for this term.
   * TODO: consider renaming this to reify?
   */
  def quote(): SExp
}

abstract class Variable() extends Term {
  import AbstractSyntax.{Subst, lookup}
  override def subst(s: Subst) = lookup(this, s)

  override def variables() = ListSet(this)

  /**
   * Produce a fresh variable.
   * We guarantee that fresh(fresh(x)) != fresh(x)
   * but not necessarily fresh(x) != fresh(y)
   */
  def fresh(): Variable
}

abstract class Application() extends Term {
  def fun: Any
  def args: List[Term]

  override def variables() = {
    ListSet.empty ++ args.flatMap(term => term.variables())
  }
  override def quote(): SExp = {
    Cons(Atom(fun), fromSeq(args.map(t => t.quote())))
  }
}

/**
 * Literal terms act as 0-ary function terms.
 */
case class Literal(val value: Any) extends Application {
  import AbstractSyntax.Subst

  override def fun = value
  override def args = Nil
  override def subst(s: Subst): Term = this

  override def quote() = Atom(value)
}

case class Apply(sym: Symbol, terms: List[Term]) extends Application {
  override def fun = sym
  override def args = terms

  import AbstractSyntax.Subst
  override def subst(s: Subst): Term = Apply(sym, terms.map(t => t.subst(s)))
}


/**
 * Unification, substitution, variable renaming, and related utilities.
 * 
 * TODO: look into prolog unification vs. sound first-order unification.
 */
object AbstractSyntax {
  /*
   * Implementation is based on:
   * 
   * Quick miniKanren-like code 
   * sokuza-kanren.scm,v 1.1 2006/05/10 23:12:41 oleg
   * http://okmij.org/ftp/Scheme/sokuza-kanren.scm
   *
   * <- http://lambda-the-ultimate.org/node/1494
   * By Ehud Lamm at 2006-05-22 08:15 
   */

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
      case a1: Application => t2 match {
	case v2: Variable => Some(s + (v2 -> t1))
	case a2: Application => {
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
      case a1: Application => data match {
	case v2: Variable => None
	case a2: Application => {
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

  def renamevars(f: Formula, todo: Set[Variable], root: Variable): Formula = {
    val (sub, _) = mksubst(todo, Nil, root, Map())
    f.subst(sub)
  }

  @tailrec
  def mksubst(todo: Iterable[Variable], done: List[Variable],
	      root: Variable, s: Subst): (Subst, List[Variable]) = {
    if (todo.isEmpty) (s, done) else {
      val vr = root.fresh()
      mksubst(todo.tail, vr :: done, root, s + (todo.head -> vr))
    }
  }
}



/* TODO: consider skolemization */
/* SCRAP:
 * largely following ACL2.
 * 
 * Matt Kaufmann and J Moore
 * A Precise Description of the ACL2 Logic
 * April, 1998.
 * http://www.cs.utexas.edu/users/moore/publications/km97a.pdf
 *
 */
/* look at defchoose in ACL2
 * 
 * Structured Theory Development for a Mechanized Logic,
 * M. Kaufmann and J Moore,
 * Journal of Automated Reasoning 26, no. 2 (2001), pp. 161-203.
 * */

