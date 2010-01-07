/*
 * RDF Abstract syntax as per 2004 Recommendation
 * http://www.w3.org/TR/2004/REC-rdf-concepts-20040210/
 */

package org.w3.swap.rdf2004

import org.w3.swap.logicalsyntax.{Formula, Equal, Not, And, Exists,
				  Term, Apply, Variable,
				  FunctionSymbol,
				  Notation}

/* Terms */
case class URI(i: String) /* ISSUE: not every string makes a URI */
          /* ISSUE: actually closer to IRI; called RDFuri or something
	   * in the spec */
     extends FunctionSymbol(0) {
  override def toString(): String = {
    "<" + i + ">"
  }
}
sealed abstract class Literal() extends FunctionSymbol(0)
case class PlainLiteral(s: String) extends Literal
case class Language(code: String) /* ISSUE: restricted to lang code syntax */
case class Text(chars: String, lang: Language) extends Literal {
  override def toString(): String = {
    "'" + chars + "'@" + lang.code
  }
}
case class DatatypedLiteral(value: String, dt: URI) extends Literal {
  override def toString(): String = {
    "'" + value + "'^^<" + dt.i + ">"
  }
}


case class BlankNode(hint: String, id: AnyRef) extends Variable {
  override def toString(): String = {
    "_:" + hint
  }
}

case class F(name: String, override val arity: Int) extends
  FunctionSymbol(arity)

class SyntaxError(msg: String) extends Exception

object Vocabulary {
  val nil: Term = URI("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")
}

/* Formulas */
object AbstractSyntax {
  val holds = F("holds", 3)

  /* checks well-formedness of Atoms */
  def triple(s: Term, p: Term, o: Term) = {
    def atom() = Not(Equal(Apply(holds, List(s, p, o)), Vocabulary.nil))

    p match {
      case Apply(_: URI, Nil) =>
	s match {
	  /* is there a scala syntax for folding 2 cases? */
	  case BlankNode(_, _) => atom()
	  case Apply(_: URI, Nil) => atom()
	  case _ => throw new SyntaxError("subject must be URI or Blank Node")
	}
      case _ => throw new SyntaxError("predicate must be URI")
    }
  }

  def add (f: Formula, s: Term, p: Term, o: Term): Formula = {
    val g = triple(s, p, o)
    val vg = g.variables

    f match {
      case Not(Equal(_, nil)) => {
	if (vg.isEmpty) { And(List(f, g)) }
	else { Exists(vg, And(List(f, g))) }
      }
      case And(fl) => {
	if (vg.isEmpty) { And(fl ++ List(g)) }
	else { Exists(vg, And(fl ++ List(g))) }
      }
      case Exists(vl, And(fl)) => {
	Exists(vl union g.variables, And(fl ++ List(g)))
      }
      case _ => throw new SyntaxError("f must be an RDF 2004 formula")
    }
  }

  /*
   * ISSUE: keep the triples sorted for ease of graph comparison?
   * use a Set rather than a list? (Set interface, ListSet impl)
   */
}

