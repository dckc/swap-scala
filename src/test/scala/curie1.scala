package org.w3.swap.test

import org.w3.swap

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class CURIEmisc extends Spec with ShouldMatchers {
  import swap.rdf.{URI, CURIE}

  describe("CURIE handler") {
    val e1 = <link rel="next" href="data:abc" />
    //val scope = new swap.rdf.XMLNameScope()
    //val base = "data:"

    it("should grok link types in @rel") {
      CURIE.refN(e1, "@rel", true) should equal (
	List(URI(CURIE.xhv + "next"))
      )
    }
  }
}
