package org.w3.swap

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class NTriplesMisc extends Spec with ShouldMatchers {
  import Walker.fmlaSexp
  import rdf2004.{BlankNode, AbstractSyntax, URI}
  import logicalsyntax.{And, FunctionSymbol, Exists}
  val triple = AbstractSyntax.triple _

  describe("substring preliminary") {
    it("never mind") {
      ("<abc>".substring(1,4)) should equal ("abc")
    }
  }

  describe("NTriples blank nodes") {

    it("should match by name"){
      val vars = List(BlankNode("", "abc".intern()),
		      BlankNode("", "<abc>".substring(1,4).intern()) )

      (vars.removeDuplicates.length) should equal(1)
    }
  }

  describe("Formula.variables()") {
    it("should remove dups"){
      val v1 = BlankNode("", "abc".intern())
      val v2 = BlankNode("", "<abc>".substring(1,4).intern())
      val f = And(List(triple(URI("data:s1"), URI("data:p"), v1),
		       triple(URI("data:s2"), URI("data:p"), v2) ))
      (f.variables()) should equal (List(v1))
      (f.variables()) should equal (List(v1, v2).removeDuplicates)
    }
  }

  describe("NTriples parser") {
    it("should grok simple n-triples") {
      val p = new org.w3.swap.NTriples()
      val doc = """
<data:bob> <data:home> _:somewhere .
_:somewhere <data:in> <data:Texas> .
"""
      val fr = p.parseAll(p.ntriplesDoc, doc)
      (fr match {
	case p.Success(f, _) => fmlaSexp(f).toString()
	case p.Failure(msg, _) => msg
	case p.Error(msg, _) => msg
      }) should equal (
	/* @@DUP vars! */
	"(exists (?IDsomewhere) (and (holds (data:bob) (data:home) ?IDsomewhere) (holds ?IDsomewhere (data:in) (data:Texas))))"
      )

      (fr match {
	case p.Success(Exists(vl, _), _) => vl.toString()
	case _ => "FAIL"
      }) should equal ( "List(_:ID)" )

    }
  }
}
