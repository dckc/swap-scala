/*
 * RDF Abstract syntax as per 2004 Recommendation
 * http://www.w3.org/TR/2004/REC-rdf-concepts-20040210/
 */

package org.w3.swap.rdf2004

import org.w3.swap.logicalsyntax.{Formula, NotNil, And, Exists,
				  Term, Apply, Variable,
				  FunctionSymbol,
				  Unifier
				}

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

/* Formulas */
object AbstractSyntax {
  val holds = F("holds", 3)

  /* checks well-formedness of Atoms */
  def triple(s: Term, p: Term, o: Term) = {
    def atom() = NotNil(Apply(holds, List(s, p, o)))

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
      case NotNil(_) => {
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


/*
 * TODO: consider Ulman's efficient algorithm for subgraph isomorphism
 * http://en.wikipedia.org/wiki/Subgraph_isomorphism
 * http://portal.acm.org/citation.cfm?doid=321921.321925
 */

  /* looked at SICP
   * http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-29.html#%_sec_4.4.4
   * didn't end up using it much.
   */

object Semantics {
  def entails(f: Formula, g: Formula): Boolean = {
    /* assumes formulas are in RDF normal form:
     * And()
     * or NotNil(term)
     * or And(f1 :: f2 :: tail)
     * or Exists(v1 :: vrest, And(fmlas))
     * */

    val data = f match {
      case NotNil(_) => List(f)
      case And(fmlas) => fmlas
      case Exists(vars, ff) => {
	throw new Exception("@@fresh vars not implemented")
      }
      case _ => {
	throw new SyntaxError("no Forall in RDF 2004")
      }
    }

    val kb = new KnowledgeBase(data.toStream)

    val answers = g match {
      case Exists(vars, gg) => kb.solve(gg)
      case _ => kb.solve(g)
    }

    ! answers.isEmpty
  }
}

class GoalRestriction(msg: String) extends Exception

class KnowledgeBase(facts: Stream[Formula]) {
  import Unifier.{Subst, unify}

  /* TODO: index kb by FunctionSymbol
   * a la:
   * type KB = Function[Set[FunctionSymbol], Stream[Formula]]
   * */
  def solve(goal: Formula): Stream[Subst] = solve(goal, Map())

  def solve(goal: Formula, s: Subst): Stream[Subst] = {
    goal match {
      case NotNil(term) => {
	facts.flatMap(f => f match {
	  case NotNil(t2) => unify(term, t2, s).toStream
	  case _ => Stream.empty
	})
      }
      case And(fmlas) => solveall(fmlas, s)
      case _ => { throw new GoalRestriction("huh? " + goal.toString()) }
    }
  }

  def solveall(goals: List[Formula], s: Subst): Stream[Subst] = {
    goals match {
      case Nil => Stream.cons(s, Stream.empty)
      case List(g) => solve(g, s)
      case g :: tail =>
	solve(g, s).flatMap(si => solveall(tail, si))
    }
  }

}
    
