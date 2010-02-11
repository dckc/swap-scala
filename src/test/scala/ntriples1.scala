package org.w3.swap.test

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import org.w3.swap
import swap.ntriples.NTriplesSyntax
import swap.logic1.Term
import swap.rdfxml
import swap.rdflogic.{Name, Plain, Data, XMLLit, TermNode }
import swap.rdfgraph.RDFGraph
import swap.sexp.{SExp, Cons, Atom}
import SExp.fromSeq


class TestParser extends NTriplesSyntax with TermNode {
  type BlankNode = swap.rdfxml.BlankNode

  val scope = new rdfxml.Scope()
  def blankNode(n: String) = scope.byName(n)

  def arcs() = Nil // don't need it for testing.

  def test(doc: String): SExp = {
    parseAll(ntripleDoc, doc) match {
      case Success(arcs, _) =>
	fromSeq(Atom('and) :: arcs.toList.map {
	  case (s, p, o) => fromSeq(List('holds,
					 List(p, s, o).map(TQ.quote)))
	})

      case Failure(_, rest) => fromSeq(List('failure,
					    Atom(rest.pos.longString)))

      case Error(_, rest) => fromSeq(List('error,
					  Atom(rest.pos.longString)))

    }
  }
}


object TQ {
  implicit def sym(s: Symbol): Atom = Atom(s)

  def quote(term: Term): SExp = {
    term match {
      case v: rdfxml.BlankNode => v.sym
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
  }
}
