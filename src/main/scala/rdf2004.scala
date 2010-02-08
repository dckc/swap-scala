package org.w3.swap.rdf

import scala.collection.immutable.ListSet

import org.w3.swap
import swap.logic.{Formula, Atomic, And, Exists,
		   Term, Variable,
		   FunctionTerm, Apply, Literal,
		   AbstractSyntax => Logic, ConjunctiveKB }
import swap.sexp.Cons
import swap.sexp.SExp.fromSeq

/**
 * The atomic formulas of RDF.
 * @param s a BlankNode or URI
 * @param p a URI
 * @param o a URI, BlankNode, or Literal from plain(), text(), data(), or xml()
 * 
 * TODO: move much of its structure up to Atomic a la FunctionTerm
 */
case class Holds(s: Term, p: Term, o: Term) extends Atomic {
  require(s match { case BlankNode(_, _) | URI(_) => true; case _ => false } )
  require(p match { case URI(_) => true; case _ => false } )
  require(AbstractSyntax.checkterm(o, true))

  override def terms() = List(s, p, o)
  def subst(sub: Logic.Subst) = {
    Holds(s.subst(sub), p.subst(sub), o.subst(sub))
  }

  override def quote() = Cons('holds, fromSeq(terms.map(t => t.quote())))
}

/**
 * Logical constants (i.e. 0-ary function terms) of RDF.
 */
case class URI(val i: String) extends FunctionTerm {
  /* ISSUE: not every string makes a URI */
  /* ISSUE: actually closer to IRI; called RDFuri or something
   * in the spec */
  override def fun = Symbol(i)
  override def args = Nil

  override def subst(s: Logic.Subst) = this
}

/**
 * Logical variables of RDF.
 * @param n: an XML name. TODO: assert this
 */
case class BlankNode(val n: String, val qual: Option[Int]) extends Variable {
  override def quote() = sym

  lazy val sym = qual match {
    case None => Symbol(n)
    case Some(x) => Symbol(n + "." + x)
  }
}

class XMLNameScope(override val vars: Iterable[Variable])
extends swap.logic.Scope(vars) {
  def this() = this(List())

  import scala.collection.mutable
  val varstack = new mutable.Stack[Variable]
  varstack.pushAll(vars)

  /**
   * @param suggestedName: an XML name
   * @return an XML name unique to this scope
   */
  override def fresh(suggestedName: String): BlankNode = {
    assert(suggestedName.length > 0)

    /* baseName is a name that does *not* follow the xyx.123 pattern */
    val baseName = {
      val lastChar = suggestedName.substring(suggestedName.length-1)
      if("0123456789".contains(lastChar) &&
	 suggestedName.contains('.')) suggestedName + "_"
      else suggestedName
    }

    val b = {
      val seen = varstack.exists { v => v.quote().toString() == baseName }
      if (seen) BlankNode(baseName, Some(varstack.size))
      else BlankNode(baseName, None)
    }

    varstack.push(b)
    b
  }
}

/**
 * RDF Abstract syntax as per 2004 Recommendation
 * http://www.w3.org/TR/2004/REC-rdf-concepts-20040210/
 */
object AbstractSyntax {
  import scala.xml.NodeSeq

  final val rdf_type = URI(Vocabulary.`type`)

  def plain(s: String): Term = Literal(s)

  /**
   * @param code a language code; must be lower case
   */
  def text(s: String, code: Symbol): Term = {
    assert({val c = code.toString(); c == c.toLowerCase})

    Apply('text, List(Literal(s), Literal(code)) )
  }
  def data(lex: String, dt: URI): Term = Apply('data, List(dt, Literal(lex)))
  def xml(x: NodeSeq): Term = Apply('xml, List(Literal(x)))

  def checkterm(t: Term, die: Boolean): Boolean = {
    t match {
      case BlankNode(_, _) => true
      /* TODO detail: URI syntax */
      case URI(_) => true
      case Literal(s: String) => true

      /* TODO detail: lang code syntax */
      case Apply('text, List(Literal(s: String),
			     Literal(code: Symbol) )) => true
      case Apply('data, List(URI(_),
			     Literal(s: String) )) => true
      case Apply('xml, List(Literal(e: NodeSeq))) => true

      case Apply(_, Nil) => true // @@allow skolem terms, for now...

      case _ => if (die) throw new Exception("bad term: " + t) else false
    }
  }

  /**
   * TODO: normalize() that can do e.g. And(And(f, g), h) => And(f, g, h)
   * then wellformed is just (a) no free vars, and (b) all terms check.
   */
  def wellformed(f: Formula): Boolean = wellformed(f, false)
  def wellformed(f: Formula, die: Boolean): Boolean = {

    def wfatom(f: Formula, die: Boolean): Boolean = {
      f match {
	case Holds(s, p, o) => true
	case _ => if (die) throw new Exception("not wfatom:" + f) else false
      }
    }

    def wfconj(fmlas: Iterable[Formula], die: Boolean): Boolean = {
      fmlas.forall(f => wfatom(f, die))
    }
    
    if (f.freevars().isEmpty) {
      f match {
	case Exists(vs, And(fmlas)) => {
	  if (vs.isEmpty) false else wfconj(fmlas, die)
	}
	case And(fmlas) => {
	  wfconj(fmlas, die)
	}
	case _ => wfatom(f, die)
      }
    } else false
  }


  /**
   * assumes all variables are in the same scope
   * preserves wff-ness
   * preserves order of Atoms in And()
   * */
  def add (f: Formula, s: Term, p: Term, o: Term): Formula = {
    wellformed(f, true)

    val g = Holds(s, p, o)
    val vg = g.variables

    f match {
      case x: Atomic => {
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
      case _ => throw new Exception("wellformed() is broken")
    }
  }

  var skolem_count = 0 // @@TODO: get rid of this or something.
  def fresh(): FunctionTerm = {
    skolem_count = skolem_count + 1
    val i = "http://www.w3.org/2000/10/swap/log#skolem@@" + skolem_count
    URI(i)
  }


  def skolemize(f: Formula, fresh: () => FunctionTerm): Formula = {
    f match {
      case Exists(vars, g) => {
	// make a substitution that maps each variable to a fresh ground term
	val pairs = vars.toList.map(v => (v, fresh()))
	val sub = Map(pairs: _*) 

	f.subst(sub)
      }
      case _ => {
	assert(f.variables().isEmpty) // just EC logic for now
	f
      }
    }
  }

  /**
   * preserved well-formedness
   * renames variables when necessary
   * TODO: split rdf well-formedness into term checking and EC formula checking.
   */
  def conjunction(f: Formula, g: Formula): Formula = {
    import Logic.mksubst

    assert(wellformed(f, true))
    assert(wellformed(g, true))

    def mkand(f: Formula, g: Formula): Formula = {
      (f, g) match {
	case (And(fl), And(gl)) => And(fl ++ gl)
	case (x: Atomic, And(gl)) => And(List(f) ++ gl)
	case (And(fl), x: Atomic) => And(fl ++ List(g))
	case (_, _) => And(List(f, g))
      }
    }

    (f, g) match {
      case (Exists(vf, ff), Exists(vg, gg)) => {
	val shared = vf filter (vg contains)
	val (vg2, g2) = if (shared.isEmpty) (vg, gg) else {
	  val scope = new XMLNameScope(f.variables() ++ g.variables())
	  val (sub, freshvars) = mksubst(shared, Nil, scope, Map())
	  val g3 = gg.subst(sub)
	  (Set() ++ freshvars ++ (vg filterNot (shared contains)), g3)
	}

	Exists(vf ++ vg2, mkand(ff, g2))
      }
      case (Exists(vf, ff), _) => {
	Exists(vf, mkand(ff, g))
      }
      case (_, Exists(vg, gg)) => {
	Exists(vg, mkand(f, gg))
      }
      case (_, _) => mkand(f, g)
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

class Graph(val arcs: Iterable[Holds]) extends ConjunctiveKB {
  import AbstractSyntax.wellformed

  override def getData(tokens: Seq[Any]) = arcs.toStream

  /**
   * Make a graph out of the atoms of a formula.
   */
  def this(f: Formula) = {
    this(
      (f match {
	case x: Atomic => List(f)
	case And(fmlas) => fmlas
	case Exists(vars, And(fmlas)) => fmlas
	case _ => throw new Exception("@@! wellformed")
      }).flatMap { // map Formula to Holds
	case h: Holds => Some(h);
	case _ => throw new Exception("require(wellformed(f))@@")
      })
  }


  def arcsMatching(s: Term, p: Term, o: Term): Stream[(Term, Term, Term)] = {
    val goal = Holds(s, p, o)
    solve(goal).map (
      solution => {
	val result = goal.subst(solution)
	(result.s, result.p, result.o)
      })
  }

  /**
   * @param s: a Term. exactly 1 of s, p, o is a Variable
   * @param p: a Term. exactly 1 of s, p, o is a Variable
   * @param o: a Term. exactly 1 of s, p, o is a Variable
   *
   * @return a Stream of matching terms.
   */
  def each(s: Term, p: Term, o: Term): Stream[Term] = {
    val goal = Holds(s, p, o)
    assert(goal.variables.size == 1)

    for(subst <- solve(goal)) yield {
      // value of 1st/only binding
      subst.valuesIterator.next
    }
  }

  /**
   * @throws Predef.NoSuchElementException if there's no such arc.
   */
  def any(s: Term, p: Term, o: Term) = each(s, p, o).head

  def contains(s: Term, p: Term, o: Term): Boolean = {
    val goal = Holds(s, p, o)

    !solve(goal).isEmpty
  }

  val vars = new XMLNameScope(arcs.flatMap { arc => arc.variables() })

  /**
   * Handy variable specific to this graph for use in queries.
   */
  val qvar = vars.fresh("Q")
}

object Semantics {
  import AbstractSyntax.{wellformed, skolemize, fresh}

  def entails(f: Formula, g: Formula): Boolean = {
    ! proofs(f, g).isEmpty
  }

  def proofs(f: Formula, g: Formula): Stream[Logic.Subst] = {
    assert(wellformed(f, true))
    assert(wellformed(g, true))

    val fmlas = f match {
      case atom: Holds => List(atom)
      case And(atoms) => atoms
      case Exists(vars, And(atoms)) => atoms
      case _ => {
	throw new Exception("wellformed() is broken")
      }
    }

    val arcs = fmlas.flatMap {
      case atom: Holds => Some(atom)
      case _ => throw new Exception("wellformed() is broken")
    }
    val kb = new Graph(arcs)

    val answers = g match {
      case Exists(vars, gg) => kb.solve(gg)
      case _ => kb.solve(g)
    }

    answers
  }
}

