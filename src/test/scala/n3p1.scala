package org.w3.swap.test

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import org.w3.swap
import swap.sexp.{SExp, Atom, Cons}
import swap.sexp.SExp.fromSeq
import swap.logic1.Term
import swap.logic1cl.Implication
import swap.rdflogic.Name
import swap.logic1n3.{N3Parser, Var, App, Quant,
		      BoolT, IntT, StringT, DoubleT, DecimalT}

class TestN3Logic(t: List[Implication]) extends TestProver(t) {
  override def quote(terms: List[Term]): SExp = {
    fromSeq(terms map {
      case Name(i) => fromSeq(List(Atom(Symbol(i))))
      case Var(i, n, true) => Atom(Symbol("?" + i))
      case Var(i, n, false) => Atom(Symbol("_:" + i + n))
      case BoolT(true) => Atom('T)
      case BoolT(false) => Atom('NIL) //hmm...
      case IntT(i) => Atom(i)
      case StringT(s) => Atom(s)
      case DoubleT(v) => Atom(v)
      case DecimalT(v) => fromSeq(List('decimal, Atom(v.toString)))
      case App(sym, args) => Cons(sym, quote(args))
      case Quant(sym, vars, t) => Cons(fromSeq(sym :: vars), quote(List(t)))
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

    fmlas foreach { f => assert(l.wff(f)) }

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
  (exists
    (_:tag:public-cwm-talk@w3.org,e0 )
    (holds (data:#x ) (data:#p ) _:tag:public-cwm-talk@w3.org,e0 )
    )
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
    (holds (data:#x ) (data:#p ) (data:#y ) )
    (holds (data:#bob ) (data:#knows ) (data:#joe ) )
    )
  (exists
    (_:tag:public-cwm-talk@w3.org,e0 )
    (holds (data:#x ) (data:#p ) _:tag:public-cwm-talk@w3.org,e0 )
    )
  )"""
      )
    }

    it("should handle nested formulas.") {
      val n3p = new N3Parser("data:")
      parsedoc(n3p, """
@keywords is, of, a.
@forAll A, s.
  { A says { A says s } } => { A says s }.
"""
	       ) should equal (
"""(implies
  (holds
    ?data:#A
    (data:#says )
    (related ?data:#A (data:#says ) ?data:#s )
    )
  (holds ?data:#A (data:#says ) ?data:#s )
  )""")
    }

    it("should grok @keywords.") {
      val n3p = new N3Parser("data:")
      parsedoc(n3p, """
@keywords is, of, a.
@prefix : <xyz#>.

pat knows joe.
""") should equal (
	"""(holds (data:xyz#pat ) (data:xyz#knows ) (data:xyz#joe ) )"""
      )
    }

    it("should grok multiple delcarations in a row.") {
      val n3p = new N3Parser("data:")
      parsedoc(n3p, """
#    ``@keywords`` to elide colons as explained in [N3Spec]_::

     @keywords is, of, a.
     @prefix : <refi_ex#>.

fido a Dog.
""") should equal (
	"""(holds (data:refi_ex#fido ) (data:refi_ex#a ) (data:refi_ex#Dog ) )"""
      )
    }


    it("should scope existentials appropriately.") {
      val n3p = new N3Parser("data:")
      parsedoc(n3p, """
@keywords is, of, a.

a b _:c.
d e _:c.
""") should equal (
"""(exists
  (_:data:#c0 )
  (and
    (holds (data:#a ) (data:#b ) _:data:#c0 )
    (holds (data:#d ) (data:#e ) _:data:#c0 )
    )
  )"""
      )
    }

    it("should make a fresh existential variable for each [] phrase.") {
      val n3p = new N3Parser("data:")
      parsedoc(n3p, """
<#pat> <#child> [ <#age> 4 ] , [ <#age> 3 ].
""") should equal (
"""(exists
  (_:tag:public-cwm-talk@w3.org,e1 _:tag:public-cwm-talk@w3.org,e0 )
  (and
    (holds _:tag:public-cwm-talk@w3.org,e0 (data:#age ) 4 )
    (holds _:tag:public-cwm-talk@w3.org,e1 (data:#age ) 3 )
    (holds (data:#pat ) (data:#child ) _:tag:public-cwm-talk@w3.org,e0 )
    (holds (data:#pat ) (data:#child ) _:tag:public-cwm-talk@w3.org,e1 )
    )
  )"""
      )
    }


  }
}

class N3ProoveMisc extends Spec with ShouldMatchers {
  import swap.webdata

  describe("N3Logic on Small Case Study in section 4 of the paper") {
    val web = ResourceOpener
    val theory = loadN3(web, "jar:/ars-coherent.n3")
    val logic = new TestN3Logic(theory)

    it("should parse the axioms"){
      theory.map(logic.quote(_).pretty()).mkString("\n") should equal (
"""(implies
  (and
    (holds
      (jar:/ars-coherent#x )
      (jar:/ars-coherent#r )
      (jar:/ars-coherent#y )
      )
    (holds
      (jar:/ars-coherent#x )
      (jar:/ars-coherent#r )
      (jar:/ars-coherent#z )
      )
    )
  (exists
    (_:jar:/ars-coherent#u0 )
    (and
      (holds
        (jar:/ars-coherent#y )
        (jar:/ars-coherent#r )
        _:jar:/ars-coherent#u0
        )
      (holds
        (jar:/ars-coherent#z )
        (jar:/ars-coherent#r )
        _:jar:/ars-coherent#u0
        )
      )
    )
  )
(implies
  (holds
    (jar:/ars-coherent#x )
    (jar:/ars-coherent#re )
    (jar:/ars-coherent#y )
    )
  (or
    (holds
      (jar:/ars-coherent#x )
      (jar:/ars-coherent#r )
      (jar:/ars-coherent#y )
      )
    (holds
      (jar:/ars-coherent#x )
      (jar:/ars-coherent#e )
      (jar:/ars-coherent#y )
      )
    )
  )
(implies
  (holds
    (jar:/ars-coherent#x )
    (jar:/ars-coherent#r )
    (jar:/ars-coherent#y )
    )
  (holds
    (jar:/ars-coherent#x )
    (jar:/ars-coherent#re )
    (jar:/ars-coherent#y )
    )
  )
(implies
  (holds
    (jar:/ars-coherent#x )
    (jar:/ars-coherent#e )
    (jar:/ars-coherent#y )
    )
  (holds
    (jar:/ars-coherent#x )
    (jar:/ars-coherent#re )
    (jar:/ars-coherent#y )
    )
  )
(holds
  (jar:/ars-coherent#x )
  (jar:/ars-coherent#e )
  (jar:/ars-coherent#x )
  )
(implies
  (holds
    (jar:/ars-coherent#x )
    (jar:/ars-coherent#e )
    (jar:/ars-coherent#y )
    )
  (holds
    (jar:/ars-coherent#y )
    (jar:/ars-coherent#e )
    (jar:/ars-coherent#x )
    )
  )
(implies
  (and
    (holds
      (jar:/ars-coherent#x )
      (jar:/ars-coherent#e )
      (jar:/ars-coherent#y )
      )
    (holds
      (jar:/ars-coherent#y )
      (jar:/ars-coherent#re )
      (jar:/ars-coherent#z )
      )
    )
  (holds
    (jar:/ars-coherent#x )
    (jar:/ars-coherent#re )
    (jar:/ars-coherent#z )
    )
  )"""
      )
    }

    val goaltheory = loadN3(web, "jar:/ars-goal.n3")

    it("should parse the goal"){
      goaltheory.map(logic.quote(_).pretty()).mkString("\n") should equal (
"""(implies
  (and
    (holds
      ?jar:/ars-instantiate#x
      (jar:/ars-coherent#re )
      ?jar:/ars-instantiate#y
      )
    (holds
      ?jar:/ars-instantiate#x
      (jar:/ars-coherent#re )
      ?jar:/ars-instantiate#z
      )
    )
  (exists
    (_:jar:/ars-instantiate#u0 )
    (and
      (holds
        ?jar:/ars-instantiate#y
        (jar:/ars-coherent#re )
        _:jar:/ars-instantiate#u0
        )
      (holds
        ?jar:/ars-instantiate#z
        (jar:/ars-coherent#re )
        _:jar:/ars-instantiate#u0
        )
      )
    )
  )"""
      )
      goaltheory.length should equal (1)
    }

    ignore("should find a derivation/proof"){
      import swap.logic1cl.Conjunction
      import swap.logic1cl.CoherentLogic.freevars

      val goal = goaltheory(0)
			 
      (logic.prove(goal) match {
	case Some(pf) => 
	  logic.quotepf(logic.linearize(pf)).toString()
	case lose => lose.toString()
      }) should equal (
	"@@"
      )
    }
  }

  def loadN3(web: swap.webdata.URLOpener, path: String): List[Implication] = {
    val addr = web.abs(path)
    val p = new N3Parser(addr)
    p.parseAll(p.document, web.open_any(addr)) match {
      case p.Success(_, _) => p.theory
      case lose => {
	System.err.println("N3Parser failed:")
	System.err.println(lose.toString)
	Nil
      }
    }
  }

}

/**
 * Opener for jar resources
 */
object ResourceOpener extends swap.webdata.URLOpener {
  import java.io.InputStreamReader

  def abs(ref: String): String = {
    assert(ref.startsWith("jar:/"))
    ref
  }

  override def open_any(addr: String): InputStreamReader = {
    assert(addr.startsWith("jar:/"))
    val path = addr.substring(4)
    val s = this.getClass.getResourceAsStream(path)
    new InputStreamReader(s)
  }
}

