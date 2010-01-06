/*
 * RDF Abstract syntax as per 2004 Recommendation
 */

package org.w3.rdf2004

import logicalsyntax.{Formula, PredicateSymbol, Constant, Variable, variables}

/* Terms */
case class URI(i: String) /* ISSUE: not every string makes a URI */
          /* ISSUE: actually closer to IRI; called RDFuri or something
	   * in the spec */
     extends FunctionSymbol {
  override def toString(): String {
    "<" + i + ">"
  }
}
sealed case class Literal() extends FunctionSymbol
case class PlainLiteral(s: String) extends Literal
case class DatatypedLiteral(value: String, dt: URI) extends Literal {
  override def toString(): String {
    "'" + chars + "'^^<" + dt.i + ">"
  }
}
case class Language(code: String) /* ISSUE: restricted to lang code syntax */
case class Text(chars: String, lang: Language) extends Literal {
  override def toString(): String {
    "'" + chars + "'@" + lang.code
  }
}

case class BlankNode(hint: String, id: AnyRef) extends Variable {
  override def toString(): String {
    "_:" + hint
  }
}

/* Formulas */
object AbstractSyntax {
  val holds = new PredicateSymbol

  def uri(i: String): Term { Apply(URI(i), Nil) }

  /* checks well-formedness of Atoms */
  def triple(s: Term, p: Term, o: Term) {
    p match {
      case Apply(_: URI, Nil) =>
	s match {
	  /* is there a scala syntax for folding 2 cases? */
	  case BlankNode => Atom(holds, [s, p, o])
	  case Apply(_: URI, Nil) => Atom(holds, [s, p, o])
	  case _ => raise SyntaxError("subject must be URI or Blank Node")
	}
      case _ => raise SyntaxError("predicate must be URI")
    }
  }

  def variables(atoms: List[Atom]): List[Variable] {
    atoms.flatMap(_ => variables(_))
  }

  def quantify(f: Formula, vars: List[Variable]): Formula {
    if (vars == Nil) { f }
    else { quantify(Exists(vars.head, f), vars.tail) }
  }
      

  def graph(triples: List[Atom]): Formula {
    if (triples == Nil) { TruthConstant(true) }
    else {
      val sorted = triples.sort(compare)
      
      def conjoin(todo: List[Atom], done: Formula): Formula {
	if (todo == Nil) { done }
	else { conjoin(todo.tail, And(todo.head, done)) }
      }
      
      quantify(conjoin(sorted.tail, sorted.head), variables(triples))
    }
  }

  val graph0 = TruthConstant(true)

  def + (graph: Formula, s, p, o: Term): Formula {
    val t = triple(s, p, o)
    val vars, conjunction = unquantify(graph)
    quantify(And(t, conjunction), vars + variables(t))
  }

  /*
   * ISSUE: keep the triples sorted for ease of graph comparison?
   */
  def compare (x: Atom, y: Atom) { compare(x.hashCode(), y.hashCode()) }

}
