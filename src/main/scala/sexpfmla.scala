/* represent RDF 2004 formulas as s-expressions */
/* ISSUE/RFE/TODO: JSON work-alike */

package org.w3.swap

import logicalsyntax.{Formula, TruthConstant, Atom, And, Exists,
		      Term, Variable, Apply}
import rdf2004.{BlankNode, URI, R, PlainLiteral, DatatypedLiteral,
		Text, Language}
import SExp.fromList

object Walker {
  def fmlaSexp(f: Formula): SExp = {
    f match {
      case Exists(v, g) =>
	List("exists", List(termSexp(v)), fmlaSexp(g))
      case And(g, h) => List("and", fmlaSexp(g), fmlaSexp(h))
      case Atom(R(name, _), terms) => Cons(Symbol(name),
					   fromList(terms.map(termSexp)) )
      case TruthConstant(true) => List("and")
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

      case _ => List("non-rdf-2004-term@@", t.toString())
    }
  }
}
