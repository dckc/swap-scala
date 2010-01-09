package org.w3.swap

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import logicalsyntax.{Formula, NotNil, And, Exists,
		      Term, Variable, Literal, Apply }

class LogicSyntax extends Spec with ShouldMatchers {
  case class V(n: Symbol) extends Variable {
    override def name = n
  }

  implicit def mkv(n: Symbol): Variable = V(n)
  implicit def mkn(n: Int): Term = Literal(n)

  describe("logical formulas") {
    val f = Exists(List('x, 'y),
		   And(List(NotNil('x),
			    NotNil('y),
			    NotNil(Apply('nil, Nil)),
			    NotNil(2),
			    NotNil(Apply('sqrt, List(4))) )) )

    it("should represent formulas"){
      (f.toString()) should equal ("Exists(List(V('x), V('y)),And(List(NotNil(V('x)), NotNil(V('y)), NotNil(Apply('nil,List())), NotNil(Literal(2)), NotNil(Apply('sqrt,List(Literal(4)))))))")
    }

    it("should find variables"){
      (f.variables.removeDuplicates) should equal (
	List(V('x), V('y))
      )
    }
  }
}

class RDFSyntax extends Spec with ShouldMatchers {
  import rdf2004.{BlankNode, URI}
  import rdf2004.AbstractSyntax.{atom, add, plain}

  describe("triples as atomic formulas") {
    val t1 = atom(URI("data:bob"), URI("data:name"), plain("Bob"))

    it ("should convert RDF triple Atoms to strings reasonably") {
      (t1.toString()) should equal (
	"NotNil(Apply('holds,List(URI(data:bob), URI(data:name), Literal(Bob))))"
      )
    }

    it ("should convert to S-Expression reasonably") {
      (t1.quote().print()) should equal (
	"(holds (data:bob) (data:name) '\"Bob\")"
      )
    }
  }

  val vhome = BlankNode("_:home", Some(1))
  val tbob: Term = URI("data:bob")
  val phome: Term = URI("data:home")
  val pin: Term = URI("data:in")
  val ttexas: Term = URI("data:Texas")

  describe("graph building") {

    val graph = add(atom(tbob, phome, vhome),
		    vhome, pin, ttexas)

    it ("should make a graph of 2 triples") {
      (graph.toString()) should equal (
	"Exists(List(BlankNode(_:home,Some(1))),And(List(NotNil(Apply('holds,List(URI(data:bob), URI(data:home), BlankNode(_:home,Some(1))))), NotNil(Apply('holds,List(BlankNode(_:home,Some(1)), URI(data:in), URI(data:Texas)))))))"
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
  import rdf2004.{BlankNode, URI}
  import rdf2004.AbstractSyntax.{plain}
  import rdf2004.Semantics.{entails, conjoin}
  import logicalsyntax.Unifier.{unify}

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

  def mkf(s: String) = new NTriples().toFormula(s)

  describe ("Semantics: Conjunction (aka merge)") {
    val and2 = conjoin(mkf("""<data:bob> <data:home> <data:x> .
			   <data:dan> <data:home> <data:Austin>."""),
		       mkf("<data:x> <data:in> <data:tx> .") )
    it("should work on this formula"){
      (and2.quote().print()) should equal(
 "(and (holds (data:bob) (data:home) (data:x)) (holds (data:dan) (data:home) (data:Austin)) (holds (data:x) (data:in) (data:tx)))"
      )
    }

    it("should do renaming when necessary"){
      val f = mkf("<data:bob> <data:home> _:somewhere .")
      ( conjoin(f, f).quote.print()
     ) should equal("(exists (_:somewhere '_:somewhere.2) (and (holds (data:bob) (data:home) _:somewhere) (holds (data:bob) (data:home) '_:somewhere.2)))")
    }
  }

  describe ("Entailment") {
    it("should handle X |= X for atomic, ground X") {
      ( entails(mkf("<data:bob> <data:home> <data:Texas> ."),
		mkf("<data:bob> <data:home> <data:Texas> ."))
     ) should equal (true)
    }
    it("should handle A |= Ex x A/x ") {
      ( entails(mkf("<data:bob> <data:home> <data:Texas> ."),
		mkf("<data:bob> <data:home> _:somewhere ."))
     ) should equal (true)
    }
    it("should *not* think that A |= B for distinct ground A, B") {
      ( entails(mkf("<data:bob> <data:home> <data:Texas> ."),
		mkf("<data:sally> <data:home> <data:Texas> ."))
     ) should equal (false)
    }

    it("should handle A^B |= Ex v (A^B)/v") {
      (entails(mkf("""<data:dan> <data:home> <data:Austin>.
		  <data:Austin> <data:in> <data:Texas>."""),
	       mkf("""<data:dan> <data:home> _:somewhere.
		  _:somewhere <data:in> <data:Texas>."""))
     ) should equal (true)
    }

    it("should *not* think that A^B |= Ex v (A^C)/v") {
      ( entails(mkf("""<data:dan> <data:home> <data:Austin>.
		    <data:Dallas> <data:in> <data:Texas>."""),
		mkf("""<data:dan> <data:home> _:somewhere.
		  _:somewhere <data:in> <data:Texas>."""))
     ) should equal (false)
    }

    it("should handle 2 bindings for v1, 1 for v2") {
      ( entails(mkf("""<data:dan> <data:home> <data:Austin>.
		    <data:bob> <data:home> <data:Austin>."""),
		mkf("""_:somebody <data:home> _:somewhere."""))
     ) should equal (true)
    }

    it("should handle variable loops, out-of-order triples") {
      ( entails(mkf("""<data:bob> <data:home> <data:Dallas>.
		    <data:bob> <data:visited> <data:Texas>.
		    <data:Dallas> <data:in> <data:Texas>."""),
		mkf("""_:somebody <data:home> _:somewhere.
		    _:somewhere <data:in> _:where.
		    _:somebody <data:visited> _:where."""))
     ) should equal (true)
    }

    it("should notice one extra character") {
      ( entails(mkf("""<data:boob> <data:home> <data:Dallas>.
		    <data:bob> <data:visited> <data:Texas>.
		    <data:Dallas> <data:in> <data:Texas>."""),
		mkf("""_:somebody <data:home> _:somewhere.
		    _:somewhere <data:in> _:where.
		    _:somebody <data:visited> _:where."""))
     ) should equal (false)
    }

  }
}
