/* Abstract syntax of logics such as First Order Logic
 * and Intuitionistic Logic
 * following ACL2 a bit more closely now...
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
      case Apply(f, terms) => {
	terms.flatMap(term => term.variables).removeDuplicates
      }
    }
  }
}

case class Variable() extends Term
case class FunctionSymbol(arity: Int)
case class Apply(f: FunctionSymbol, terms: List[Term]) extends Term
/* TODO: check f.arity vs terms.length */

/* make 0-ary function terms easier to use */
object FunctionSymbol {
  implicit def makeConstant(f: FunctionSymbol): Term = Apply(f, Nil)
}

sealed abstract class Formula() {
  def variables(): List[Variable] = {
    this match {
      case NotNil(x) => x.variables()
      case And(fl) => fl.flatMap(fmla => fmla.variables).removeDuplicates
      case Exists(vl, g) => vl union g.variables
      case Forall(vl, g) => vl union g.variables
    }
  }
}

case class NotNil(x: Term) extends Formula
case class And(fmlas: List[Formula]) extends Formula

/* TODO: consider skolemization */
/* look at defchoose in ACL2
 * 
 * Structured Theory Development for a Mechanized Logic,
 * M. Kaufmann and J Moore,
 * Journal of Automated Reasoning 26, no. 2 (2001), pp. 161-203.
 * */
case class Exists(vars: List[Variable], f: Formula) extends Formula
case class Forall(vars: List[Variable], f: Formula) extends Formula

/* TODO: consider http://en.wikipedia.org/wiki/Intuitionistic_logic */
// object bottom: Formula
// perhaps def bottom(t) => Not(Equal(t, t))

//object Notation {
  // not needed yet, and certainly not tested:
  // def or(f: Formula, g: Formula) { Not(And(Not(f), Not(g))) }
  // def implies(f: Formula, g: Formula) { Not(And(Not(f), g)) }
//}
