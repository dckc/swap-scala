package org.w3.swap.logic0

/**
 * Shoenfield's rules of propositional calculus [83]
 * from Milawa.
 */

abstract class PropositionalCalculus {
  //@@TODO: abstract class Formula
  import org.w3.swap
  import swap.logic.{Formula, Unary, Binary}

  def wff(f: Formula): Boolean

  case class Or(A: Formula, B: Formula) extends Binary(A, B){
    import swap.sexp.SExp.fromSeq
    import swap.logic.AbstractSyntax.Subst
    def quote() = fromSeq(List(Symbol("POR*"), A.quote(), B.quote()))
    def subst(s: Subst) = Or(A.subst(s), B.subst(s))
  }
  case class Not(A: Formula) extends Unary(A){
    import swap.sexp.SExp.fromSeq
    import swap.logic.AbstractSyntax.Subst
    def quote() = fromSeq(List(Symbol("PNOT*"), A.quote()))
    def subst(s: Subst) = Not(A.subst(s))
  }



  /**
   * From ((A or B) or C) derive (A or (B or C))
   */
  def associativity(pi: List[Formula], q: Formula): Boolean = {
    q match {
      case Or(Or(a, b), c) => pi match {
	case List(Or(aa, Or(bb, cc))) => (a == aa && b == bb && c == cc)
	  
	  case _ => false
      }
      case _ => false
    }
  }


  /**
   * From (A or B) derive A
   */
  def contraction(pi: List[Formula], q: Formula): Boolean = {
    pi match {
      case List(Or(a, aa)) => q == a && a == aa
      case _ => false
    }
  }

  /**
   * From (A or B) (-A or C) derive (B or C)
   */
  def cut(pi: List[Formula], q: Formula): Boolean = {
    q match {
      case Or(b, c) => pi match {
	case List(Or(a, bb), Or(Not(aa), cc)) =>
	  a == aa && b == bb && c == cc
	case _ => false
      }
      case _ => false
    }
  }

  /**
   * From B derive (A or B)
   */
  def expansion(pi: List[Formula], q: Formula): Boolean = {
    q match {
      case Or(a, b) => pi match {
	case List(bb) => bb == b && wff(a)
	case _ => false
      }
      case _ => false
    }
  }

  /**
   * From nothing derive (-A or A)
   */
  def propositional_schema(pi: List[Formula], q: Formula): Boolean = {
    q match {
      case Or(Not(a), aa) => a == aa && pi == Nil
      case _ => false
    }
  }

}
