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
  val l = new TestN3Logic(Nil)

  def parsedoc(p: N3Parser, doc: String): String = {
    val fmlas = p.parseAll(p.document, doc) match {
      case p.Success(_, _) => p.theory
      case lose => {
	System.err.println("N3Parser failed:")
	System.err.println(lose.toString)
	Nil
      }
    }

    fmlas.map(l.quote(_).pretty()).mkString("\n")
  }

  describe("N3 Parser") {

    it("should do the 'pat knows joe' case.") {
      val n3p = new N3Parser("data:")
      parsedoc(n3p, "<#pat> <#knows> <#joe>.") should equal (
	"""(holds (data:#pat ) (data:#knows ) (data:#joe ) )"""
      )
    }

    /* TODO: reset variable scope at end of fmla
    it("should keep adding formulas.") {
      val n3p = new N3Parser("data:")
      parsedoc(n3p, "<#pat> <#knows> _:somebody.")

      parsedoc(n3p, "<#joe> <#knows> _:somebody.") should equal (
	"""(related (data:#pat ) (data:#knows ) (data:#joe ) )@@"""
      )
    }
    */
    it("should handle implication.") {
      val n3p = new N3Parser("data:")
      parsedoc(n3p, "{ <#x> <#p> <#y> } => { <#x> <#p> [] } .") should equal (
	"""(implies
  (holds (data:#x ) (data:#p ) (data:#y ) )
  (holds (data:#x ) (data:#p ) "_:tag:public-cwm-talk@w3.org,e0" )
  )"""
      )
    }

    it("should handle multiple atoms in an implication.") {
      val n3p = new N3Parser("data:")
      parsedoc(n3p, """
{ <#x> <#p> <#y>.
  <#bob> <#knows> <#joe>.
 } => { <#x> <#p> [] } .
""") should equal (
	"""(implies
  (and
    (holds (data:#bob ) (data:#knows ) (data:#joe ) )
    (holds (data:#x ) (data:#p ) (data:#y ) )
    )
  (holds (data:#x ) (data:#p ) "_:tag:public-cwm-talk@w3.org,e0" )
  )"""
      )
    }
  }
}
