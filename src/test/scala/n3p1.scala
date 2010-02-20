package org.w3.swap.test

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import org.w3.swap
import swap.sexp.{SExp, Atom, Cons}
import swap.sexp.SExp.fromSeq
import swap.logic1.Term
import swap.logic1cl.Implication
import swap.rdflogic.Name
import swap.logic1n3.{N3Parser, Var, App,
		      BoolT, IntT, StringT, DoubleT, DecimalT}

class TestN3Logic(t: List[Implication]) extends TestLogic(t) {
  override def quote(terms: List[Term]): SExp = {
    fromSeq(terms map {
      case Name(i) => fromSeq(List(Atom(Symbol(i))))
      case Var(_, i, n, true) => Atom("?" + i)
      case Var(_, i, n, false) => Atom("_:" + i + n)
      case BoolT(true) => Atom('T)
      case BoolT(false) => Atom('NIL) //hmm...
      case IntT(i) => Atom(i)
      case StringT(s) => Atom(s)
      case DoubleT(v) => Atom(v)
      case DecimalT(v) => fromSeq(List('decimal, Atom(v.toString)))
      case App(sym, args) => Cons(sym, quote(args))
      case t => Atom(t) // shouldn't happen
    })
  }
}

class N3ParseMisc extends Spec with ShouldMatchers {
  describe("N3 Parser") {
    val n3p = new N3Parser("data:")

    it("should do the 'pat knows joe' case.") {
      val fmlas = n3p.parseAll(n3p.document,
			       "<#pat> <#knows> <#joe>.") match {
	case n3p.Success(_, _) => n3p.theory
	case lose => {
	  System.err.println("N3Parser failed:")
	  System.err.println(lose.toString)
	  Nil
	}
      }
      val l = new TestN3Logic(fmlas)
      fmlas.map(l.quote(_).pretty()).mkString("\n") should equal (
	"""(related (data:#pat ) (data:#knows ) (data:#joe ) )"""
      )
    }
  }
}
