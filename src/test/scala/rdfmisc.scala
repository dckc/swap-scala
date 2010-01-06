package org.w3.swap.rdf2004
import org.w3.swap.logicalsyntax.Atom

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class Misc extends Spec with ShouldMatchers {
  import rdf2004.AbstractSyntax

  describe("triples as atomic formulas") {
    it ("should make an atom out of uri, uri, plain literal") {
      AbstractSyntax.triple(URI("x:bob"),
			    URI("x:name"),
			    PlainLiteral("Bob")) match {
	case Atom(_, _) => true
	case _ => false
      }
    }
  }
}
