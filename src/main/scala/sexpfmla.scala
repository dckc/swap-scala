/* represent RDF 2004 formulas as s-expressions */
/* ISSUE/RFE/TODO: JSON work-alike */

package org.w3.swap

import logicalsyntax.{Formula, NotNil, And, Exists,
		      Term, Variable, Apply}
import rdf2004.{BlankNode, URI,
		PlainLiteral, DatatypedLiteral,
		Text, Language}
import SExp.fromList

object Walker {
  def fmlaSexp(f: Formula): SExp = {
    f match {
      case NotNil(term) => termSexp(term)
      case And(fl) => List(Symbol("and")) ++ fl.map(fmlaSexp)
      case Exists(vl, g) =>
	List("exists", vl.map(termSexp), fmlaSexp(g))
      case _ => List("non-rdf-2004-formula@@", f.toString())
    }
  }

  def termSexp(t: Term): SExp = {
    t match {
      case BlankNode(hint, id) =>
	Symbol("?" + hint + id.toString())

      case Apply(URI(i), Nil) => List(i)
      case Apply(PlainLiteral(s), Nil) => "'" + s + "'" /* TODO: escaping */
      case Apply(Text(c, Language(code)), Nil) =>
	List("text", "'" + c + "'", code)
      case Apply(DatatypedLiteral(s, URI(dt)), Nil) =>
	List("data", "'" + s + "'", List(dt))

      case Apply(holds, List(s, p, o)) =>
	List("holds", termSexp(s), termSexp(p), termSexp(o))

      case _ => List("non-rdf-2004-term@@", t.toString())
    }
  }
}
