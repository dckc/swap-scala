package org.w3.swap.logic1

import scala.annotation.tailrec

import org.w3.swap
import swap.logic.Formula // TODO: move to logic1

case class Appeal(method: Symbol, conclusion: Formula,
		  subproofs: List[Appeal], extras: List[Any])

object CoreProof {

  // TODO: consider Set rather than List for axioms, thms
  def appeal_step_ok(x: Appeal,
		     axioms: List[Formula], thms: List[Formula],
		     structure: ComputationalLogic): Boolean = {

    // TODO: equality, induction, base eval, ...
    val rules = Map('ASSOCIATIVITY -> structure.associativity _,
		    'CONTRACTION -> structure.contraction _,
		    'CUT -> structure.cut _,
		    'EXPANSION -> structure.expansion _,
		    'PROPOSITIONAL_SCHEMA ->
		    structure.propositional_schema _
		  )

    x match {
      case Appeal('AXIOM, f, Nil, Nil) =>
	axioms.contains(f) && structure.wff(f)

      case Appeal('THEOREM, f, Nil, Nil) =>
	thms.contains(f) && structure.wff(f)

      case Appeal(name, f, subproofs, Nil) if rules.isDefinedAt(name) =>
	rules(name)(subproofs.map(_.conclusion), f)

      case _ => false
    }
  }

  def proofp(x: List[Appeal],
	     axioms: List[Formula], thms: List[Formula],
	     structure: ComputationalLogic): Boolean = {
    @tailrec
    def proof1n(x: Either[Appeal, List[Appeal]]): Boolean = {
      x match {
	case Left(x1) => {
	  appeal_step_ok(x1, axioms, thms, structure) &&
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
