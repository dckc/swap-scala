/*
 * RDF Abstract syntax as per 2004 Recommendation
 * http://www.w3.org/TR/2004/REC-rdf-concepts-20040210/
 */

package org.w3.swap.rdf

import org.w3.swap
import swap.logic.{Formula, Atomic, And, Exists,
		   Term, Variable,
		   Application, Apply, Literal,
		   AbstractSyntax => Logic
		 }

case class NotNil(x: Term) extends Atomic {
  import Logic.Subst

  def variables() = x.variables()
  def freevars() = x.variables()
  def subst(s: Subst) = NotNil(x.subst(s))

  /* This sorta promotes terms to formulas, which is like ACL2.
   * But *unlike* ACL2, (and ...) is a formula, not a term,
   * and the formulas below it may be quantified. Hmm.*/
  def quote() = x.quote()
}

/* Terms */
case class URI(val i: String) extends Application {
  /* ISSUE: not every string makes a URI */
  /* ISSUE: actually closer to IRI; called RDFuri or something
   * in the spec */
  override def fun = Symbol(i)
  override def args = Nil

  import Logic.Subst
  override def subst(s: Subst) = this
}

case class BlankNode(val n: String, val qual: Option[Any]) extends Variable {
  override def name: Symbol = {
    qual match {
      case None => Symbol(n)
      case Some(x) => Symbol(n + "." + x.toString())
    }
  }

  /* For debuggin sanity etc., let's be sure that variables that
   * look the same compare equal.
   * TODO: consider moving this constraint up to Variable. */
  override def equals(that: Any): Boolean = {
    that match {
      case b: BlankNode => name == b.name
      case _ => false
    }
  }
  override def hashCode(): Int = name.hashCode()
}

class SyntaxError(msg: String) extends Exception

object AbstractSyntax {
  final val rdf_type = URI(Vocabulary.`type`)

  def wellformed(f: Formula): Boolean = {
    f match {
      case Exists(vs, g) => {
	wfconj(g, vs)
      }
      case And(_) => wfconj(f, Nil)
      case NotNil(_) => wfatom(f, Nil)
      case _ => false
    }
  }

  def wfconj(f: Formula, bound: List[Variable]): Boolean = {
    f match {
      case And(fmlas) => fmlas.forall(g => wfatom(g, bound))
      case NotNil(_) => wfatom(f, bound)
      case _ => false
    }
  }

  def wfatom(f: Formula, bound: List[Variable]): Boolean = {
    f match {
      case NotNil(Apply('holds, List(s, p, o))) => {
	((f.variables() filterNot (bound contains)).isEmpty
	 && checkterm(s) && checkterm(p) && checkterm(o))
      }
      case _ => false
    }
  }

  def checkterm(t: Term): Boolean = {
    t match {
      case BlankNode(_, _) => true
      /* TODO detail: URI syntax */
      case URI(_) => true
      case Literal(s: String) => true

      /* TODO detail: lang code syntax */
      case Apply('text, List(Literal(s: String),
			     Literal(code: String) )) => true
      case Apply('data, List(URI(_),
			     Literal(s: String) )) => true
      case Apply('xml, List(Literal(s: String))) => true

      case _ => false
    }
  }

  def plain(s: String): Term = Literal(s)
  def text(s: String, code: String): Term = Apply('text,
						  List(Literal(code),
						       Literal(s)))
  def data(lex: String, dt: URI): Term = Apply('data, List(dt, Literal(lex)))
  def xml(lex: String): Term = Apply('xml, List(Literal(lex)))

  /**
   * holds(s, p, o) is a term that is not nil iff s is related to o by p.
   *
   * TODO: consider using a distinct type for Holds.
   * TODO: consider using an Either type to constrain s to URI, BlankNode
   * TODO: consider constraining p to URI. (but keep in mind generalized
   *       graphs in the RDFS sense.)
   */
  def holds(s: Term, p: Term, o: Term) = Apply('holds, List(s, p, o))

  /* TODO: test that this produces only wffs or exceptions */
  def atom(s: Term, p: Term, o: Term) = {
    /* scalaQ: use lazy value instead of def? */
    def atom() = NotNil(holds(s, p, o))

    p match {
      case URI(_) =>
	s match {
	  case BlankNode(_, _) | URI(_) => atom()
	  case _ => throw new SyntaxError("subject must be URI or Blank Node")
	}
      case _ => throw new SyntaxError("predicate must be URI")
    }
  }

  /* preserves wff-ness
   * preserves order of Atoms in And()
   * assumes all variables are in the same scope
   * */
  def add (f: Formula, s: Term, p: Term, o: Term): Formula = {
    val g = atom(s, p, o)
    val vg = g.variables

    f match {
      case NotNil(_) => {
	if (vg.isEmpty) { And(List(f, g)) }
	else { Exists(vg.toList, And(List(f, g))) }
      }
      case And(fl) => {
	if (vg.isEmpty) { And(fl ++ List(g)) }
	else { Exists(vg.toList, And(fl ++ List(g))) }
      }
      case Exists(vl, And(fl)) => {
	Exists(vl ++ g.variables, And(fl ++ List(g)))
      }
      case _ => throw new SyntaxError("f must be an RDF 2004 formula")
    }
  }
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

/* TODO: move this to swap.logic.ExistentialConjuctiveFragment
 * cf. http://en.wikipedia.org/wiki/Conjunctive_query
 * */
object Semantics {
  import Logic.{renamevars, mksubst}
  import AbstractSyntax.wellformed

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
	/* skolemize the variables in the antecedent */
	/* should use functions rather than variables,
	 * but as long as they're fresh, it doesn't matter.
	 * */
	List(renamevars(ff, vars.toList))
      }
      case _ => {
	/* TODO: check well-formedness in general. */
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


  /* TODO: probably better named conjunction */
  def conjoin(f: Formula, g: Formula): Formula = {
    assert(wellformed(f))
    assert(wellformed(g))

    (f, g) match {
      case (Exists(vf, ff), Exists(vg, gg)) => {
	val shared = vf.toList intersect vg.toList
	val (vg2, g2) = if (shared.isEmpty) (vg, gg) else {
	  val sub = mksubst(shared, Nil, Map())
	  val g3 = gg.subst(sub)
	  val vg3: List[Variable] = for {
	    t <- sub.valuesIterator.toList
	    if t.isInstanceOf[Variable]
	  } yield t.asInstanceOf[Variable]

	  (vg3, g3)
	}

	Exists(vf ++ vg2, conjoin2(ff, g2))
      }
      case (Exists(vf, ff), _) => {
	Exists(vf, conjoin2(ff, g))
      }
      case (_, _) => conjoin2(f, g)
    }
  }

  protected def conjoin2(f: Formula, g: Formula): Formula = {
    (f, g) match {
      case (And(fl), And(gl)) => And(fl ++ gl)
      case (x: Atomic, And(gl)) => And(List(f) ++ gl)
      case (And(fl), x: Atomic) => And(fl ++ List(g))
      case (_, _) => And(List(f, g))
    }
  }
}

class GoalRestriction(msg: String) extends Exception

class KnowledgeBase(facts: Stream[Formula]) {
  import Logic.{Subst, unify}

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

  def solveall(goals: Iterable[Formula], s: Subst): Stream[Subst] = {
    goals match {
      case Nil => Stream.cons(s, Stream.empty)
      case List(g) => solve(g, s)
      case g :: tail =>
	solve(g, s).flatMap(si => solveall(tail, si))
    }
  }

}
    
