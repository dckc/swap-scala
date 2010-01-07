package org.w3.swap.rdf2004

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class LogicSyntax extends Spec with ShouldMatchers {
  import logicalsyntax.{Formula, Equal, Not, And, Exists,
			Variable, FunctionSymbol, Apply }

  case class V(name: String) extends Variable
  case class F(name: String) extends FunctionSymbol(0)

  implicit def mkv(n: String) = V(n)
  implicit def mkn(n: Int) = Apply(F(n.toString()), Nil)

  describe("logical formulas") {
    val f = Exists(List("x", "y"),
		   And(List(Not(Equal("x", 2)),
			    Equal("y", Apply(F("nil"), Nil)),
			    Equal(2, Apply(F("sqrt"), List(4))) )) )

    it("should represent formulas"){
      (f.toString()) should equal ("Exists(List(V(x), V(y)),And(List(Not(Equal(V(x),Apply(F(2),List()))), Equal(V(y),Apply(F(nil),List())), Equal(Apply(F(2),List()),Apply(F(sqrt),List(Apply(F(4),List())))))))")
    }

    it("should find variables"){
      (f.variables) should equal (
	List(V("x"), V("y"))
      )
    }
  }
}

class Misc extends Spec with ShouldMatchers {
  import AbstractSyntax.{triple, add}
  import Walker.{fmlaSexp, termSexp}

  describe("triples as atomic formulas") {
    val t1 = triple(URI("x:bob"), URI("x:name"), PlainLiteral("Bob"))

    it ("should convert RDF triple Atoms to strings reasonably") {
      (t1.toString()) should equal (
	"Not(Equal(Apply(F(holds,3),List(Apply(<x:bob>,List()), Apply(<x:name>,List()), Apply(PlainLiteral(Bob),List()))),Apply(<http://www.w3.org/1999/02/22-rdf-syntax-ns#nil>,List())))"
      )
    }

    it ("should convert to S-Expression reasonably") {
      (fmlaSexp(t1).toString()) should equal (
	"(holds (x:bob) (x:name) 'Bob')"
      )
    }
  }

  describe("graph building") {
    val vhome = BlankNode("home", "1")
    val tbob = URI("x:bob")
    val phome = URI("x:home")
    val pin = URI("x:in")
    val ttexas = URI("x:Texas")

    val graph = add(triple(tbob, phome, vhome),
		    vhome, pin, ttexas)

    it ("should make a graph of 2 triples") {
      (graph.toString()) should equal (
	"Exists(List(_:home),And(List(Not(Equal(Apply(F(holds,3),List(Apply(<x:bob>,List()), Apply(<x:home>,List()), _:home)),Apply(<http://www.w3.org/1999/02/22-rdf-syntax-ns#nil>,List()))), Not(Equal(Apply(F(holds,3),List(_:home, Apply(<x:in>,List()), Apply(<x:Texas>,List()))),Apply(<http://www.w3.org/1999/02/22-rdf-syntax-ns#nil>,List()))))))"
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
