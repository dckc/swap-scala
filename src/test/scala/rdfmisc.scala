package org.w3.swap.rdf2004
import org.w3.swap.logicalsyntax.Atom

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class Misc extends Spec with ShouldMatchers {
  import rdf2004.AbstractSyntax

  describe("triples as atomic formulas") {
    val t1 = AbstractSyntax.triple(URI("x:bob"),
				   URI("x:name"),
				   PlainLiteral("Bob"))

    it ("should make an atom out of uri, uri, plain literal") {
      (t1 match {
	case Atom(_, _) => true
	case _ => false
      }) should equal (true)
    }

    it ("should convert RDF triple Atoms to strings reasonably") {
      (t1.toString()) should equal ("Atom(PredicateSymbol(holds),List(Apply(<x:bob>,List()), Apply(<x:name>,List()), Apply(PlainLiteral(Bob),List())))")
    }
  }
}
