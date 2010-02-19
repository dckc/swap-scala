package org.w3.swap.test

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import org.w3.swap
import swap.webdata.RDFaParser
import swap.rdflogic.{Name, XMLVar, Scope}

class RDFaMisc extends Spec with ShouldMatchers {

  describe("XML variable scope") {
    val s = new Scope(Nil)
    it("should make distinct fresh vars") {
      (s.fresh("x").qual == s.fresh("x").qual) should equal (false)
    }

    it("should find vars by name") {
      (s.byName("x") == s.byName("x")) should equal (true)
    }
  }
  describe("RDFa walker") {

    it("should stop chaining on bogus rel values (Test #105) ") {
      val e1 = <div
      xmlns:dc="http://purl.org/dc/elements/1.1/"
      about="" 
      rel="dc:creator">
      <a rel="myfoobarrel" href="ben.html">Ben</a> created this page.
      </div>

      var addr = "data:"
      val undef = RDFaParser.undef
      var arcs = RDFaParser.walk(e1, addr, Name(addr), undef, Nil, Nil, null)
      (arcs.force.head match {
	case (Name(_), Name(_), XMLVar(_, _)) => true
	case _ => false
      }) should equal (true)
    }
  }
}
