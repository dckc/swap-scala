package org.w3.swap.test

import org.w3.swap
import swap.rdfa.{CURIE => CURIEx}
import swap.logic1.Variable
import swap.rdflogic.TermNode
import swap.rdfxml

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

object CURIE extends CURIEx with TermNode {
  type BlankNode = Variable

  val vars = new rdfxml.Scope()
  def fresh(hint: String) = vars.fresh(hint)
  def byName(name: String) = vars.byName(name)
}

class CURIE1misc extends Spec with ShouldMatchers {

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
	List(CURIE.uri(CURIE.xhv + "next"))
      )
    }

    it("should grok empty prefix @rel") {
      val e2 = <link rel=":next" href="data:0064.xhtml" />

      CURIE.refN(e2, "@rel", true) should equal (
	List(CURIE.uri(CURIE.xhv + "next"))
      )
    }

    it("should skip bogus rel values") {
      val elt = <a rel="myfoobarrel" href="ben.html">Ben</a>
      CURIE.refN(elt, "@rel", true) should equal (
	List()
      )
    }

    it("should normalize case of link types") {
      val elt = <a rel="NEXT" href="ben.html">Ben</a>
      CURIE.refN(elt, "@rel", true) should equal (
	List(CURIE.uri(CURIE.xhv + "next"))
      )

      val elt2 = <a rel="NeXT" href="ben.html">Ben</a>
      CURIE.refN(elt2, "@rel", true) should equal (
	List(CURIE.uri(CURIE.xhv + "next"))
      )

      val elt3 = <a rel="neXt" href="ben.html">Ben</a>
      CURIE.refN(elt3, "@rel", true) should equal (
	List(CURIE.uri(CURIE.xhv + "next"))
      )
    }

    it("should skip undeclared prefixes") {
      val elt = <p property="dc:test">Test</p>
      CURIE.refN(elt, "@property", false) should equal (
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
