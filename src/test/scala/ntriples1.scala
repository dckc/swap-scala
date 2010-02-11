package org.w3.swap.test

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import org.w3.swap
import swap.webdata.NTriplesParser
import swap.logic1.Term
import swap.rdfxml
import rdfxml.XMLVar
import swap.rdflogic.{Name, Plain, Data, XMLLit, TermNode }
import swap.sexp.{SExp, Cons, Atom}
import SExp.fromSeq


class TestParser extends NTriplesParser {
  def test(doc: String): SExp = {
    parseAll(ntripleDoc, doc) match {
      case Success(arcs, _) => {
	val exprs: List[SExp] = arcs.toList.map {
	  case (s, p, o) =>
	    fromSeq(List('holds, List(p, s, o).map(TQ.quote)))
	}
	fromSeq(Atom('and) :: exprs)
      }

      case Failure(_, rest) => fromSeq(List('failure,
					    Atom(rest.pos.longString)))

      case Error(_, rest) => fromSeq(List('error,
					  Atom(rest.pos.longString)))

    }
  }

  def arcs(doc: String): Stream[Arc] = {
    parseAll(ntripleDoc, doc) match {
      case Success(arcs, _) => arcs
      case _ => Stream.empty
    }
  }
}


object TQ {
  implicit def sym(s: Symbol): Atom = Atom(s)

  def quote(term: Term): SExp = {
    term match {
      case v: XMLVar => v.sym
      case Name(n) => fromSeq(List(Symbol(n)))
      case Plain(s, None) => Atom(s)
      case Plain(s, Some(code)) => fromSeq(List('text, Atom(s), Atom(code)))
      case Data(lex, dt) => fromSeq(List('data, Atom(lex), quote(dt)))
      case XMLLit(content) => Atom(content.toString())
      case _ => Atom(Symbol("*oops*"))
    }
  }

}

class NTriplesMisc extends Spec with ShouldMatchers {
  import org.w3.swap

  describe("NTriples parser") {
      val doc = """#
<data:bob> <data:home> _:somewhere .
_:somewhere <data:in> <data:Texas> .
"""
    it("should grok simple n-triples") {
      val p = new TestParser()

      val exp = p.test(doc)
      (exp.pretty()) should equal (
	"""(and
  (holds ((data:home ) (data:bob ) somewhere ) )
  (holds ((data:in ) somewhere (data:Texas ) ) )
  )"""
      )

    }

    it("should work with SimpleSerializer") {
      val p = new NTriplesParser()

      // TODO: use stringwriter; check output
      val wr = new java.io.StringWriter()
      val arcs = p.arcs(p.parseAll(p.ntripleDoc, doc))
      rdfxml.SimpleSerializer.writeArcsDoc(wr, arcs)
      wr.close()
      wr.toString() should equal (
	"""<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
<rdf:Description rdf:about="data:bob" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"><ns0:home rdf:nodeID="somewhere" xmlns:ns0="data:"></ns0:home></rdf:Description>
<rdf:Description rdf:nodeID="somewhere" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"><ns0:in rdf:resource="data:Texas" xmlns:ns0="data:"></ns0:in></rdf:Description>
</rdf:RDF>
"""
	)
    }
  }
}
