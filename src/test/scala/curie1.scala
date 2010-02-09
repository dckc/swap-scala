package org.w3.swap.test

import org.w3.swap

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class CURIE1 extends Spec with ShouldMatchers {
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

    it("should skip bogus rel values") {
      val elt = <a rel="myfoobarrel" href="ben.html">Ben</a>
      CURIE.refN(elt, "@rel", true) should equal (
	List()
      )
    }

    it("should not allow _ to be declared as a namespace prefix") {
      val e = <p xmlns:_="http://example.org/" property="_:test">Test</p>
      CURIE.refN(e, "@property", false) should equal (
	List()
      )
    }
  }
}
