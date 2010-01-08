package org.w3.swap

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import logicalsyntax.{Formula, NotNil, And, Exists,
		      Term, Variable, FunctionSymbol, Apply }
import Walker.{fmlaSexp, termSexp}

class LogicSyntax extends Spec with ShouldMatchers {
  case class V(name: String) extends Variable
  case class F(name: String) extends FunctionSymbol(0)

  implicit def mkv(n: String) = V(n)
  implicit def mkn(n: Int) = Apply(F(n.toString()), Nil)

  describe("logical formulas") {
    val f = Exists(List("x", "y"),
		   And(List(NotNil("x"),
			    NotNil("y"),
			    NotNil(Apply(F("nil"), Nil)),
			    NotNil(2),
			    NotNil(Apply(F("sqrt"), List(4))) )) )

    it("should represent formulas"){
      (f.toString()) should equal ("Exists(List(V(x), V(y)),And(List(NotNil(V(x)), NotNil(V(y)), NotNil(Apply(F(nil),List())), NotNil(Apply(F(2),List())), NotNil(Apply(F(sqrt),List(Apply(F(4),List())))))))")
    }

    it("should find variables"){
      (f.variables) should equal (
	List(V("x"), V("y"))
      )
    }
  }
}

class RDFSyntax extends Spec with ShouldMatchers {
  import rdf2004.{BlankNode, URI, PlainLiteral}
  import rdf2004.AbstractSyntax.{triple, add, holds}

  describe("triples as atomic formulas") {
    val t1 = triple(URI("x:bob"), URI("x:name"), PlainLiteral("Bob"))

    it ("should convert RDF triple Atoms to strings reasonably") {
      (t1.toString()) should equal (
	"NotNil(Apply(F(holds,3),List(Apply(<x:bob>,List()), Apply(<x:name>,List()), Apply(PlainLiteral(Bob),List()))))"
      )
    }

    it ("should convert to S-Expression reasonably") {
      (fmlaSexp(t1).toString()) should equal (
	"(holds (x:bob) (x:name) 'Bob')"
      )
    }
  }

  val vhome = BlankNode("home", "1")
  val tbob: Term = URI("x:bob")
  val phome: Term = URI("x:home")
  val pin: Term = URI("x:in")
  val ttexas: Term = URI("x:Texas")

  describe("graph building") {

    val graph = add(triple(tbob, phome, vhome),
		    vhome, pin, ttexas)

    it ("should make a graph of 2 triples") {
      (graph.toString()) should equal (
	"Exists(List(_:home),And(List(NotNil(Apply(F(holds,3),List(Apply(<x:bob>,List()), Apply(<x:home>,List()), _:home))), NotNil(Apply(F(holds,3),List(_:home, Apply(<x:in>,List()), Apply(<x:Texas>,List())))))))"
      )
    }

    it ("should convert to S-Expression reasonably") {
      (fmlaSexp(graph).toString()) should equal (
	"(exists (?home1) (and (holds (x:bob) (x:home) ?home1) (holds ?home1 (x:in) (x:Texas))))"
      )
    }

    val vwho = BlankNode("who", "2")
    val gmore = add(add(graph,
			tbob, URI("x:friend"), vwho),
		    vwho, phome, vhome)

    it ("should handle a bit larger graph") {
      (fmlaSexp(gmore).toString()) should equal (
	"(exists (?who2 ?home1) (and (holds (x:bob) (x:home) ?home1) (holds ?home1 (x:in) (x:Texas)) (holds (x:bob) (x:friend) ?who2) (holds ?who2 (x:home) ?home1)))"
      )
    }

  }
}

class RDFSemantics extends Spec with ShouldMatchers {
  import rdf2004.{BlankNode, URI, PlainLiteral}
  import rdf2004.AbstractSyntax.holds
  import logicalsyntax.Unifier.{unify}

  val vhome = BlankNode("home", "1")
  val tbob: Term = URI("x:bob")
  val phome: Term = URI("x:home")
  val pin: Term = URI("x:in")
  val ttexas: Term = URI("x:Texas")

  describe("Unification") {
    val t1 = Apply(holds, List(tbob, phome, vhome))
    val t2 = Apply(holds, List(tbob, phome, ttexas))
    ( unify(t1, t2, Map()) ) should equal (
      Some(Map(vhome -> ttexas))
    )
  }
}
