package org.w3.swap.rdf2004
import org.w3.swap.logicalsyntax.Atom

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class Misc extends Spec with ShouldMatchers {
  import AbstractSyntax.{triple, graph, graph0, add}
  import Walker.{fmlaSexp, termSexp}

  describe("triples as atomic formulas") {
    val t1 = triple(URI("x:bob"),
				   URI("x:name"),
				   PlainLiteral("Bob"))

    it ("should make an atom out of uri, uri, plain literal") {
      (t1 match {
	case Atom(_, _) => true
	case _ => false
      }) should equal (true)
    }

    it ("should convert RDF triple Atoms to strings reasonably") {
      (t1.toString()) should equal ("Atom(R(holds,3),List(Apply(<x:bob>,List()), Apply(<x:name>,List()), Apply(PlainLiteral(Bob),List())))")
    }

    it ("should convert to S-Expression reasonably") {
      (fmlaSexp(t1).toString()) should equal ("(holds (x:bob) (x:name) 'Bob')")
    }
  }

  describe("graph building") {
    val vhome = BlankNode("home", "1")
    val tbob = URI("x:bob")
    val phome = URI("x:home")
    val pin = URI("x:in")
    val ttexas = URI("x:Texas")

    val graph = add(add(graph0, tbob, phome, vhome),
		    vhome, pin, ttexas)

    it ("should make a graph of 2 triples") {
      (graph.toString()) should equal (
	"Exists(_:home,And(Atom(R(holds,3),List(_:home, Apply(<x:in>,List()), Apply(<x:Texas>,List()))),Atom(R(holds,3),List(Apply(<x:bob>,List()), Apply(<x:home>,List()), _:home))))")
    }

    it ("should convert to S-Expression reasonably") {
      (fmlaSexp(graph).toString()) should equal (
	"(exists (?home1) (and (holds ?home1 (x:in) (x:Texas)) (holds (x:bob) (x:home) ?home1)))"
      )
    }

    val vwho = BlankNode("who", "2")
    val gmore = add(add(graph,
			tbob, URI("x:friend"), vwho),
		    vwho, phome, vhome)

    it ("should handle a bit larger graph") {
      (fmlaSexp(gmore).toString()) should equal (
	"(exists (?home1) (exists (?who2) (and (holds ?who2 (x:home) ?home1) (and (holds (x:bob) (x:friend) ?who2) (and (holds ?home1 (x:in) (x:Texas)) (holds (x:bob) (x:home) ?home1))))))"
      )
    }

  }
}
