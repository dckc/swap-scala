package org.w3.swap.test

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import org.w3.swap

class LogicSyntax extends Spec with ShouldMatchers {
  import swap.logic.{Formula, And, Exists, Atomic,
		     Term, Variable, Literal, Apply }
  case class NotNil(x: Term) extends Atomic {
    override def terms = List(x)
    override def variables = x.variables()
    override def freevars = variables()
    override def quote() = x.quote()
    override def subst(sub: swap.logic.AbstractSyntax.Subst) = (
      NotNil(x.subst(sub)) )
  }

  case class V(n: Symbol) extends Variable {
    override def fresh() = V(Symbol(n.name + this.hashCode()))
    override def quote() = n
  }

  implicit def mkv(n: Symbol): Variable = V(n)
  implicit def mkn(n: Int): Term = Literal(n)

  describe("logical formulas") {
    val f = Exists(Set() ++ List[Variable]('x, 'y),
		   And(List(NotNil('x),
			    NotNil('y),
			    NotNil(Apply('nil, Nil)),
			    NotNil(2),
			    NotNil(Apply('sqrt, List(4))) )) )

    it("should represent formulas"){
      (f.toString()) should equal ("(exists (x y) (and x y (nil) 2 (sqrt 4)))")
    }

    it("should find variables"){
      (f.variables.toList.removeDuplicates) should equal (
	List(V('x), V('y))
      )
    }
  }
}

class RDFSyntax extends Spec with ShouldMatchers {
  import swap.logic.Term
  import swap.rdf.{BlankNode, URI, Holds}
  import swap.rdf.AbstractSyntax.{add, plain}

  describe("triples as atomic formulas") {
    val t1 = Holds(URI("data:bob"), URI("data:name"), plain("Bob"))

    it ("should convert RDF triple Atoms to strings reasonably") {
      (t1.toString()) should equal (
	"(holds (data:bob) (data:name) \"Bob\")"
      )
    }

    it ("should convert to S-Expression reasonably") {
      (t1.quote().print()) should equal (
	"(holds (data:bob) (data:name) \"Bob\")"
      )
    }
  }

  val vhome = BlankNode("_:home", Some(1))
  val tbob: Term = URI("data:bob")
  val phome: Term = URI("data:home")
  val pin: Term = URI("data:in")
  val ttexas: Term = URI("data:Texas")

  describe("graph building") {

    val graph = add(Holds(tbob, phome, vhome),
		    vhome, pin, ttexas)

    it ("should make a graph of 2 triples") {
      (graph.toString()) should equal (
	"(exists (_:home.1) (and (holds (data:bob) (data:home) _:home.1) (holds _:home.1 (data:in) (data:Texas))))"
      )
    }

    it ("should convert to S-Expression reasonably") {
      (graph.quote().print()) should equal (
	"(exists (_:home.1) (and (holds (data:bob) (data:home) _:home.1) (holds _:home.1 (data:in) (data:Texas))))"
      )
    }

    val vwho = BlankNode("who", Some(2))
    val gmore = add(add(graph,
			tbob, URI("data:friend"), vwho),
		    vwho, phome, vhome)

    it ("should handle a bit larger graph") {
      (gmore.quote().print()) should equal (
	"(exists (who.2 _:home.1) (and (holds (data:bob) (data:home) _:home.1) (holds _:home.1 (data:in) (data:Texas)) (holds (data:bob) (data:friend) who.2) (holds who.2 (data:home) _:home.1)))"
      )
    }

  }
}

class RDFSemantics extends Spec with ShouldMatchers {
  import swap.rdf.{BlankNode, URI}
  import swap.rdf.AbstractSyntax.{plain, conjunction}
  import swap.ntriples.NTriplesParser
  import swap.rdf.Semantics.{entails}
  import swap.logic.{Term, Apply, And}
  import swap.logic.AbstractSyntax.unify

  val vhome = BlankNode("home", Some(1))
  val tbob: Term = URI("data:bob")
  val phome: Term = URI("data:home")
  val pin: Term = URI("data:in")
  val ttexas: Term = URI("data:Texas")

  describe("Unification") {
    val t1 = Apply('holds, List(tbob, phome, vhome))
    val t2 = Apply('holds, List(tbob, phome, ttexas))
    ( unify(t1, t2, Map()) ) should equal (
      Some(Map(vhome -> ttexas))
    )
  }

  def mkf(s: String) = new NTriplesParser().toFormula(s)

  describe ("Semantics: Conjunction (aka merge)") {
    val and2 = conjunction(mkf("""#
<data:bob> <data:home> <data:x> .
<data:dan> <data:home> <data:Austin>.
"""),
		       mkf("<data:x> <data:in> <data:tx> .\n") )
    it("should result in a conjuction of 3 atoms"){
      and2 match {
	case And(fmlas) => fmlas.toList.length should equal (3)
	case _ => and2 should not equal (And(List()))
      }
    }
    it("should work on this formula"){
      (and2.quote().print()) should equal(
 "(and (holds (data:bob) (data:home) (data:x)) (holds (data:dan) (data:home) (data:Austin)) (holds (data:x) (data:in) (data:tx)))"
      )
    }

    it("should do renaming when necessary"){
      val f = mkf("<data:bob> <data:home> _:somewhere .\n")
      ( conjunction(f, f).variables.size
     ) should equal( 2 )
    }
  }

  describe ("Entailment") {
    it("should handle X |= X for atomic, ground X") {
      ( entails(mkf("<data:bob> <data:home> <data:Texas> .\n"),
		mkf("<data:bob> <data:home> <data:Texas> .\n"))
     ) should equal (true)
    }
    it("should handle A |= Ex x A/x ") {
      ( entails(mkf("<data:bob> <data:home> <data:Texas> .\n"),
		mkf("<data:bob> <data:home> _:somewhere .\n"))
     ) should equal (true)
    }
    it("should *not* think that A |= B for distinct ground A, B") {
      ( entails(mkf("<data:bob> <data:home> <data:Texas> .\n"),
		mkf("<data:sally> <data:home> <data:Texas> .\n"))
     ) should equal (false)
    }

    it("should handle A^B |= Ex v (A^B)/v") {
      (entails(mkf("""#
<data:dan> <data:home> <data:Austin>.
<data:Austin> <data:in> <data:Texas>.
"""),
	       mkf("""#
<data:dan> <data:home> _:somewhere.
 _:somewhere <data:in> <data:Texas>.
"""))
     ) should equal (true)
    }

    it("should *not* think that A^B |= Ex v (A^C)/v") {
      ( entails(mkf("""#
<data:dan> <data:home> <data:Austin>.
<data:Dallas> <data:in> <data:Texas>.
"""),
		mkf("""#
<data:dan> <data:home> _:somewhere.
_:somewhere <data:in> <data:Texas>.
"""))
     ) should equal (false)
    }

    it("should handle 2 bindings for v1, 1 for v2") {
      ( entails(mkf("""#
<data:dan> <data:home> <data:Austin>.
<data:bob> <data:home> <data:Austin>.
"""),
		mkf("_:somebody <data:home> _:somewhere.\n"))
     ) should equal (true)
    }

    it("should not bind the same var to 2 terms") {
      ( entails(mkf("""#
<data:dan> <data:home> <data:Austin>.
<data:bob> <data:home> <data:Texas>.
"""
		  ),
		mkf("""
<data:dan> <data:home> _:somewhere.
<data:bob> <data:home> _:somewhere.
"""
		  ))
     ) should equal (false)
    }

    it("should handle variable loops, out-of-order triples") {
      ( entails(mkf("""#
<data:bob> <data:home> <data:Dallas>.
<data:bob> <data:visited> <data:Texas>.
<data:Dallas> <data:in> <data:Texas>.
"""
		  ),
		mkf("""#
_:somebody <data:home> _:somewhere.
_:somewhere <data:in> _:where.
 _:somebody <data:visited> _:where.
"""
		  ))
     ) should equal (true)
    }

    it("should notice one extra character") {
      ( entails(mkf("""#
<data:boob> <data:home> <data:Dallas>.
<data:bob> <data:visited> <data:Texas>.
<data:Dallas> <data:in> <data:Texas>.
"""
		  ),
		mkf("""#
_:somebody <data:home> _:somewhere.
 _:somewhere <data:in> _:where.
_:somebody <data:visited> _:where.
"""
		  ))
     ) should equal (false)
    }

  }
}
