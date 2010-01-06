/* Abstract syntax of logics such as First Order Logic
 * and Intuitionistic Logic */

package org.w3.swap.logicalsyntax

/* cf http://en.wikipedia.org/wiki/First-order_logic#Terms */
sealed case class Term()
case class Variable() extends Term
case class FunctionSymbol()
case class Apply(f: FunctionSymbol, terms: List[Term]) extends Term

/* make 0-ary function terms easier to use */
object FunctionSymbol {
  implicit def makeConstant(f: FunctionSymbol): Term = Apply(f, Nil)
}

/* http://en.wikipedia.org/wiki/First-order_logic#Formulas */
sealed case class Formula()
case class TruthConstant(b: Boolean) extends Formula
case class PredicateSymbol(name: String)
case class Atom(p: PredicateSymbol, terms: List[Term]) extends Formula
case class Equals(x: Term, y: Term) extends Formula
case class Not(f: Formula) extends Formula
case class And(f: Formula, g: Formula) extends Formula
case class Exists(v: Variable, f: Formula) extends Formula
case class Forall(v: Variable, f: Formula) extends Formula

/* TODO: consider http://en.wikipedia.org/wiki/Intuitionistic_logic */
// object bottom: Formula

object Notation {
  def or(f: Formula, g: Formula) { Not(And(Not(f), Not(g))) }
  def implies(f: Formula, g: Formula) { Not(And(Not(f), g)) }

  def variables(a: Atom): List[Variable] = {
    a.terms flatMap {case v : Variable => List(v); case _ => Nil}
  }
}


