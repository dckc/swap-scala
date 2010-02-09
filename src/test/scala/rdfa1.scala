package org.w3.swap.test

import org.w3.swap

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class RDFaMisc extends Spec with ShouldMatchers {
  import swap.rdf.{RDFaSyntax, XMLNameScope, URI, Holds, BlankNode}

  describe("RDFa walker") {

    it("should @@") {
      val e1 = <div
      xmlns:dc="http://purl.org/dc/elements/1.1/"
      about="" 
      rel="dc:creator">
      <a rel="myfoobarrel" href="ben.html">Ben</a> created this page.
      </div>

      var addr = "data:"
      var arcs = RDFaSyntax.walk(e1, addr, new XMLNameScope(),
				 URI(addr), null, Nil, Nil, null)
      (arcs.force.head match {
	case Holds(URI(_), URI(_), BlankNode(_, _)) => true
	case _ => false
      }) should equal (true)
    }
  }
}
