/* Abstract syntax of a First Order Logic
 * largely following ACL2.
 * 
 * Matt Kaufmann and J Moore
 * A Precise Description of the ACL2 Logic
 * April, 1998.
 * http://www.cs.utexas.edu/users/moore/publications/km97a.pdf
 *
 * see also
 * cf http://en.wikipedia.org/wiki/First-order_logic#Terms
 * and
 * http://en.wikipedia.org/wiki/First-order_logic#Formulas
 * */

package org.w3.swap.logicalsyntax

sealed abstract class Term() {
  def variables(): List[Variable] = {
    this match {
      case v: Variable => List(v)
      case a: Application => {
	a.args.flatMap(term => term.variables())
      }
    }
  }

  def quote(): SExp = {
    this match {
      case v: Variable => v.name
      case c: Literal => List('quote, c.value)
      case a: Application => a.fun :: a.args.map(t => t.quote())
    }
  }
}

abstract class Variable() extends Term {
  def name: Symbol
}

abstract class Application() extends Term {
  def fun: Any
  def args: List[Term]
}

case class Literal(val value: Any) extends Application {
  override def fun = value
  override def args = Nil
}

case class Apply(sym: Symbol, terms: List[Term]) extends Application {
  override def fun = sym
  override def args = terms
}



sealed abstract class Formula() {
  def variables(): List[Variable] = {
    this match {
      case NotNil(x) => x.variables()
      case And(fl) => fl.flatMap(fmla => fmla.variables)
      case Exists(vl, g) => vl ++ g.variables
      case Forall(vl, g) => vl ++ g.variables
    }
  }

  def quote(): SExp = {
    this match {
      /* This sorta promotes terms to formulas, which is like ACL2.
       * But *unlike* ACL2, (and ...) is a formula, not a term,
       * and the formulas below it may be quantified. Hmm.*/
      case NotNil(term) => term.quote()
      case And(fl) => 'and :: fl.map(f => f.quote())
      case Exists(vl, g) => List('exists,
				 vl.removeDuplicates.map(t => t.quote()),
				 g.quote())
      case Forall(vl, g) => List('forall,
				 vl.removeDuplicates.map(t => t.quote()),
				 g.quote())
    }
  }
}

/* reifying Not(Equal(term, NIL)) in scala is tedious...
 * perhaps an abstract Atom class of formulas? */
case class NotNil(x: Term) extends Formula

case class And(fmlas: List[Formula]) extends Formula
//object Notation {
  // not needed yet, and certainly not tested:
  // def or(f: Formula, g: Formula) { Not(And(Not(f), Not(g))) }
  // def implies(f: Formula, g: Formula) { Not(And(Not(f), g)) }
//}

case class Exists(vars: List[Variable], f: Formula) extends Formula
case class Forall(vars: List[Variable], f: Formula) extends Formula

object Unifier {
  /*
   * Quick miniKanren-like code 
   * sokuza-kanren.scm,v 1.1 2006/05/10 23:12:41 oleg
   * http://okmij.org/ftp/Scheme/sokuza-kanren.scm
   *
   * <- http://lambda-the-ultimate.org/node/1494
   * By Ehud Lamm at 2006-05-22 08:15 
   */

  type Subst = Map[Variable, Term]

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
}

/* TODO: consider skolemization */
/* look at defchoose in ACL2
 * 
 * Structured Theory Development for a Mechanized Logic,
 * M. Kaufmann and J Moore,
 * Journal of Automated Reasoning 26, no. 2 (2001), pp. 161-203.
 * */

