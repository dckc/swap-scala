package org.w3.swap.logic

import org.w3.swap
import swap.sexp.{SExp, Atom, Cons}
import swap.sexp.SExp.fromList

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

  def variables(): Iterable[Variable]

  def freevars(): Iterable[Variable]

  /* TODO: don't make a new formula unless we have to. */
  def subst(s: Subst): Formula

  def quote(): SExp

  override def toString() = quote().print()
}

abstract class Atomic extends Formula
abstract class Unary(f: Formula) extends Formula
abstract class Binary(f: Formula, g: Formula) extends Formula
abstract class Nary(val connective: Symbol,
		    val fl: Iterable[Formula]) extends Formula {
  override def quote() = Cons(connective,
			      fromList(fl.toList.map(f => f.quote())) )

  override def variables() = fl.flatMap(fmla => fmla.variables())
  override def freevars() = fl.flatMap(fmla => fmla.freevars())
}

case class And(val fmlas: Iterable[Formula]) extends Nary('and, fmlas){
  override def subst(s: AbstractSyntax.Subst) = {
    And(fmlas.map(fmla => fmla.subst(s)))
  }
}

sealed abstract class Quantified extends Formula {
  val vars: List[Variable]
  val f: Formula

  protected val head: Symbol

  def variables() = vars ++ f.variables()
  def freevars() = f.freevars().filter(v => vars.contains(v))

  def quote() = fromList(List(head,
			      vars.removeDuplicates.map(v => v.quote()),
			      f.quote()))
}

case class Exists(override val vars: List[Variable],
		  override val f: Formula) extends Quantified {
  /**
   * assume the vars in s don't occur in vars.
   * i.e. you can only substitute for free variables.
   * TODO: assert this. */
  def subst(s: AbstractSyntax.Subst) = Exists(vars, f.subst(s))

  override val head = 'exists
}

case class Forall(override val vars: List[Variable],
		  override val f: Formula) extends Quantified {
  /**
   * assume the vars in s don't occur in vars.
   * i.e. you can only substitute for free variables.
   * TODO: assert this. */
  import AbstractSyntax.Subst
  def subst(s: Subst) = Forall(vars, f.subst(s))

  override val head = 'forall
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
  def variables(): Iterable[Variable]

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

  override def variables() = List(this)

  def name: Symbol
  override def quote() = name
}

abstract class Application() extends Term {
  def fun: Any
  def args: List[Term]

  override def variables() = args.flatMap(term => term.variables())
  override def quote(): SExp = {
    Cons(Atom(fun), fromList(args.map(t => t.quote())))
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
  override def subst(s: Subst): Term = {
    /* don't make a new term unless we have to */
    /* oops; premature optimization. */
    def each(tl: List[Term]): (Boolean, List[Term]) = {
      tl match {
	case Nil => (false, Nil)
	case t1 :: rest => {
	  val x = t1.subst(s)
	  val (hit, rest2) = each(rest)

	  if (t1.eq(x)) (hit, t1 :: rest2)
	  else (true, x :: rest2)
	}
      }
    }
    val (hit, terms2) = each(terms)
    if (hit) Apply(sym, terms2)
    else this
  }
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
    if (t1 eq t2) Some(s)
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

  def unifyall(tl1: List[Term], tl2: List[Term], s: Subst): Option[Subst] = {
    (tl1, tl2) match {
      case (Nil, Nil) => Some(s)
      case (Nil, _) => None
      case (_, Nil) => None
      case (head1 :: tail1, head2 :: tail2) => {
	unify(head1, head2, s) match {
	  case None => None
	  case Some(ss) => unifyall(tail1, tail2, ss)
	}
      }
    }
  }

  def renamevars(f: Formula, vl: List[Variable]): Formula = {
    f.subst(mksubst(vl, Nil, Map()))
  }

  def mksubst(todo: List[Variable], taken: List[Variable], s: Subst): Subst = {
    todo match {
      case Nil => s
      case v :: vrest => {
	val vr = rename(v, taken)
	mksubst(vrest, vr :: taken, s + (v -> vr))
      }
    }
  }

  case class Var(n: Symbol) extends Variable {
    override def name = n
  }

  def rename(v: Variable, taken: List[Variable]): Variable = {
    val names = taken.map(v => v.name)
    val pfx = v.name
    def trynext(n: Int): Variable = {
      val name = Symbol(pfx.name + "." + n.toString())
      if (names.indexOf(name) < 0) Var(name)
      else trynext(n+1)
    }
    trynext(2)
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

