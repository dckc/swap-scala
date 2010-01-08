/*
 * RDF Abstract syntax as per 2004 Recommendation
 * http://www.w3.org/TR/2004/REC-rdf-concepts-20040210/
 */

package org.w3.swap.rdf2004

import org.w3.swap.logicalsyntax.{Formula, NotNil, And, Exists,
				  Term, Variable,
				  Application, Apply, Literal,
				  Unifier
				}

/* Terms */
case class URI(val i: String) extends Application {
  /* ISSUE: not every string makes a URI */
  /* ISSUE: actually closer to IRI; called RDFuri or something
   * in the spec */
  override def fun = Symbol(i)
  override def args = Nil
}

case class BlankNode(val n: String, val qual: Option[Any]) extends Variable {
  override def name: Symbol = {
    qual match {
      case None => Symbol(n)
      case Some(x) => Symbol(n + "." + x.toString())
    }
  }
}

class SyntaxError(msg: String) extends Exception

/* Formulas */
object AbstractSyntax {
  def plain(s: String): Term = Literal(s)
  def text(s: String, code: String): Term = Apply('text,
						  List(Literal(code),
						       Literal(s)))
  def data(lex: String, dt: URI): Term = Apply('data, List(dt, Literal(lex)))
  def xml(lex: String): Term = Apply('xml, List(Literal(lex)))

  /* checks well-formedness of Atoms */
  def atom(s: Term, p: Term, o: Term) = {
    /* scalaQ: use lazy value instead of def? */
    def atom() = NotNil(Apply('holds, List(s, p, o)))

    p match {
      case URI(_) =>
	s match {
	  /* is there a scala syntax for folding 2 cases? */
	  case BlankNode(_, _) => atom()
	  case URI(_) => atom()
	  case _ => throw new SyntaxError("subject must be URI or Blank Node")
	}
      case _ => throw new SyntaxError("predicate must be URI")
    }
  }

  /* preserves normal form
   * preserves order of Atoms in And() */
  def add (f: Formula, s: Term, p: Term, o: Term): Formula = {
    val g = atom(s, p, o)
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
	Exists(vl ++ g.variables, And(fl ++ List(g)))
      }
      case _ => throw new SyntaxError("f must be an RDF 2004 formula")
    }
  }

  /*
   * TODO: wff() checker, normalize()
   */

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
    
