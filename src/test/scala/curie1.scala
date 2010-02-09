package org.w3.swap.test

import org.w3.swap

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class CURIEmisc extends Spec with ShouldMatchers {
  import swap.rdf.{URI, CURIE}

  describe("CURIE handler") {

    it("should expand :next to xhv:next") {
      val e1 = <link rel=":next" href="data:abc" />

      CURIE.expand("", "next", e1) should equal (
	CURIE.xhv + "next")
    }

    it("should grok link types in @rel") {
      val e1 = <link rel="next" href="data:abc" />
      //val scope = new swap.rdf.XMLNameScope()
      //val base = "data:"

      CURIE.refN(e1, "@rel", true) should equal (
	List(URI(CURIE.xhv + "next"))
      )
    }

    it("should grok empty prefix @rel") {
      val e2 = <link rel=":next" href="data:0064.xhtml" />

      CURIE.refN(e2, "@rel", true) should equal (
	List(URI(CURIE.xhv + "next"))
      )
    }
  }
}
