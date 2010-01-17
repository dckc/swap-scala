package org.w3.swap.rdf

import scala.collection.immutable.ListSet

import org.w3.swap
import swap.logic.{Formula, Atomic, And, Exists,
		   Term, Variable,
		   Application, Apply, Literal,
		   AbstractSyntax => Logic, ConjunctiveKB }
import swap.sexp.Cons
import swap.sexp.SExp.fromSeq

/**
 * The atomic formulas of RDF.
 * @param s a BlankNode or URI
 * @param p a URI
 * @param o a URI, BlankNode, or Literal from plain(), text(), data(), or xml()
 * 
 * TODO: move much of its structure up to Atomic a la Application
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
case class URI(val i: String) extends Application {
  /* ISSUE: not every string makes a URI */
  /* ISSUE: actually closer to IRI; called RDFuri or something
   * in the spec */
  override def fun = Symbol(i)
  override def args = Nil

  override def subst(s: Logic.Subst) = this
}

/**
 * Logical variables of RDF.
 */
case class BlankNode(val n: String, val qual: Option[Any]) extends Variable {
  override def quote() = name

  lazy val name = qual match {
    case None => Symbol(n)
    case Some(x) => Symbol(n + "." + x.toString())
  }

  override def fresh() = BlankNode(n, Some(new Thing()))
}
object Counter {
  protected var cur = 0
  def next(): Int = { cur += 1; cur }
}
class Thing {
  private val id = Counter.next()
  override def toString = id.toString()
}


/**
 * RDF Abstract syntax as per 2004 Recommendation
 * http://www.w3.org/TR/2004/REC-rdf-concepts-20040210/
 */
object AbstractSyntax {
  import scala.xml.NodeSeq

  final val rdf_type = URI(Vocabulary.`type`)

  def plain(s: String): Term = Literal(s)
  def text(s: String, code: String): Term = Apply('text,
						  List(Literal(code),
						       Literal(s)))
  def data(lex: String, dt: URI): Term = Apply('data, List(dt, Literal(lex)))
  def xml(x: NodeSeq): Term = Apply('xml, List(Literal(x)))

  def checkterm(t: Term, die: Boolean): Boolean = {
    t match {
      case BlankNode(_, _) => true
      /* TODO detail: URI syntax */
      case URI(_) => true
      case Literal(s: String) => true

      /* TODO detail: lang code syntax */
      /* TODO: canonical case of lang code */
      case Apply('text, List(Literal(s: String),
			     Literal(code: String) )) => true
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

  def fresh(): Application = {
    val i = "http://www.w3.org/2000/10/swap/log#skolem@@" + new Thing()
    URI(i)
  }


  def skolemize(f: Formula, fresh: () => Application): Formula = {
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
	  val (sub, freshvars) = mksubst(shared, Nil, 
					 shared.iterator.next, Map())
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
    assert(goal.variables.size == 1)

    !solve(goal).isEmpty
  }

  /**
   * Handy variable specific to this graph for use in queries.
   */
  val qvar = BlankNode("Q", None).fresh()
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
      case Exists(_, _) => {
	val fskol = skolemize(f, fresh _ )
	fskol match {
	  case Exists(vars, And(atoms)) => atoms
	  case _ => throw new Exception("skolemize() is broken")
	}
      }
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

