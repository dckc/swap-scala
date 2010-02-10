package org.w3.swap.logic0

import scala.annotation.tailrec

/**
 * Formal System @@rest of sentence here.
 *
 * @@cite wikipedia
 * 
 * http://www.w3.org/XML/9711theory/FormalSystem
 * FormalSystem.lsl,v 1.4 2003/10/01 07:25:52 connolly
 */
abstract class FormalSystem {
  def wff(f: Formula): Boolean
  // YAGNI? def wff(fs: Set[Formula]): Boolean = fs.forall(wff_)

  /**
   * Constraint:
   * axiom(f) => wff(f)
   */
  def axiom(f: Formula): Boolean

  val methods: List[Symbol] // or Set? or ListSet?
  type Rule = (List[Formula], Formula) => Boolean
  def rule(method: Symbol): Rule

  def appeal_step_ok(x: Appeal, thms: List[Formula]): Boolean

  def proofp(x: List[Appeal], thms: List[Formula]): Boolean = {
    @tailrec
    def proof1n(x: Either[Appeal, List[Appeal]]): Boolean = {
      x match {
	case Left(x1) => {
	  appeal_step_ok(x1, thms) &&
	  proof1n(Right(x1.subproofs))
	}
	case Right(xn) => xn match {
	  case car :: cdr => proof1n(Left(car)) && proof1n(Right(cdr))
	  case Nil => true
	}
      }
    }

    proof1n(Right(x))
  }

}

abstract class Formula // TODO: or abstract type member of FormalSystem?

/**
 * a la Milawa
 * 
 * TODO: consider a more C-H/explicit approach, using proof terms
 */
case class Appeal(method: Symbol, conclusion: Formula,
		  subproofs: List[Appeal], extras: List[Any])

