package org.w3.swap.logic0

/**
 * Shoenfield's rules of propositional calculus [83]
 * from Milawa.
 */
abstract class PropositionalCalculus extends FormalSystem {
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

  val methods = List('ASSOCIATIVITY, 'CONTRACTION, 'CUT, 'EXPANSION,
		     'PROPOSITIONAL_SCHEMA)

  def rule(method: Symbol): Rule = {
    assert(methods contains method)
    // TODO: consider moving rules in here and using match
    val rules = List(associativity _, contraction _, cut _, expansion _,
		     propositional_schema _)
    rules(methods.indexOf(method))
  }

  // TODO: perhaps def implies(a, b) = Or(Not(a), b)
  // TODO: perhaps def and(a, b) = Not(Or(Not(a), Not(b)))
}

case class Or(a: Formula, b: Formula) extends Formula
case class Not(a: Formula) extends Formula

