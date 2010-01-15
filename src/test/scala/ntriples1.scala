package org.w3.swap.test

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class NTriplesMisc extends Spec with ShouldMatchers {
  import org.w3.swap
  import swap.rdf.{BlankNode, URI, Holds}
  import swap.logic.{And, Exists}
  import swap.ntriples.NTriplesParser

  describe("NTriples blank nodes") {

    it("should match by name"){
      val vars = List(BlankNode("", Some("abc".intern())),
		      BlankNode("", Some("<abc>".substring(1,4).intern())) )

      (vars.removeDuplicates.length) should equal(1)
    }
  }

  describe("Formula.variables()") {
    it("should expect caller to remove dups"){
      val v1 = BlankNode("", Some("abc".intern()))
      val v2 = BlankNode("", Some("<abc>".substring(1,4).intern()))
      val f = And(List(Holds(URI("data:s1"), URI("data:p"), v1),
		       Holds(URI("data:s2"), URI("data:p"), v2) ))
      (f.variables().toList.removeDuplicates) should equal (List(v1))
      (f.variables().toList.removeDuplicates) should equal (
	List(v1, v2).removeDuplicates)
    }
  }

  describe("NTriples parser") {
      val doc = """#
<data:bob> <data:home> _:somewhere .
_:somewhere <data:in> <data:Texas> .
"""
    it("should grok simple n-triples") {
      val p = new NTriplesParser()

      val fr = p.parseAll(p.ntripleDoc, doc)
      (fr match {
	case p.Success(f, _) => f.quote().print()
	case x: p.Failure => x.toString()
	case x: p.Error => x.toString()
      }) should equal (
	"(exists (_:somewhere) (and (holds (data:bob) (data:home) _:somewhere) (holds _:somewhere (data:in) (data:Texas))))"
      )

      (fr match {
	case p.Success(Exists(vl, _), _) => vl.toString()
	case _ => "FAIL"
      }) should equal ( "Set(BlankNode(_:somewhere,None))" )

    }

    it("should have a decent API") {
      val f = new NTriplesParser().toFormula(doc)

      (f.quote().print()) should equal(
	"(exists (_:somewhere) (and (holds (data:bob) (data:home) _:somewhere) (holds _:somewhere (data:in) (data:Texas))))"
      )
    }
  }
}
