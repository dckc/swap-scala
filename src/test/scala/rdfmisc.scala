package org.w3.swap.rdf2004
import logicalsyntax.Atom

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class Misc extends Spec with ShouldMatchers {

  describe("triples as atomic formulas") {
    it ("should make an atom out of uri, uri, plain literal") {
      triple(URI("x:bob"), URI("x:name"), PlainLiteral("Bob")) match {
	case Atom => true
	case _ => false
      }
    }
  }
}
