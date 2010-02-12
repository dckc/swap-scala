package org.w3.swap.webdata

import java.io.InputStreamReader
import scala.xml.XML
import scala.util.parsing.combinator.Parsers

import org.w3.swap
import swap.rdfxml
import swap.rdfa
import swap.logic1.Variable
import swap.logic1ec.{ECFormula, Exists, And, Atomic}
import swap.rdflogic.{RDFXMLTerms, TermNode, Scope, XMLVar,
		      Name, Plain, Data, XMLLit}
import swap.sexp.{SExp, Cons, Atom}
import SExp.fromSeq


/**
 * WebData can read RDF in various formats using rdflogic terms as nodes.
 */
object WebData extends TermNode {

  // TODO: conneg
  def loadRDFXML(addr: String): Stream[Arc] = {
    val base = cwdbased(addr)
    val e = XML.load(base)
    XMLtoRDFlogic.getArcs(e, base)
  }

  protected def content(addr: String): InputStreamReader = {
    val conn = new java.net.URL(addr).openConnection()
    new InputStreamReader(conn.getInputStream())
  }

  /**
   * @throws IOException if openConnection(addr) throws one
   */
  def loadNT(addr: String): Stream[Arc] = {
    val p = new NTriplesParser()
    p.arcs(p.parseAll(p.ntripleDoc, content(addr)))
  }

  /**
   * @throws IOException if openConnection(addr) throws one
   */
  def loadTurtle(addr: String): Stream[Arc] = {
    val abs = cwdbased(addr)
    loadTurtle(abs, abs)
  }

  /**
   * @throws IOException if openConnection(addr) throws one
   */
  def loadTurtle(addr: String, base: String): Stream[Arc] = {
    val p = new TurtleParser(base)
    p.arcs(p.parseAll(p.turtleDoc, content(addr)))
  }

  def loadSPARQL(addr: String): Stream[Arc] = {
    val p = new SPARQLParser(addr)

    p.arcs(p.parseAll(p.AskQuery, content(addr)))
  }
    
  def loadRDFa(addr: String): Stream[Arc] = {
    // TODO: suggest media type with Accept: headers; check Content-type
    val e = XML.load(addr)
    val b1 = cwdbased(addr)
    val base = e \ "head" \ "base" \ "@href"
    RDFaParser.getArcs(e, if (base.isEmpty) b1 else base.text)
  }

  /**
   * Absolutize a URI reference w.r.t. cwd.
   */
  def cwdbased(ref: String): String = {
    val cwd = java.lang.System.getProperty("user.dir")
    new java.io.File(cwd).toURI().resolve(ref).toString()
  }
}

object XMLtoRDFlogic extends rdfxml.XMLtoRDF with RDFXMLTerms

object RDFaParser extends rdfa.RDFaSyntax with RDFXMLTerms

trait ConcreteParser extends Parsers with RDFXMLTerms {

  def arcs(result: this.ParseResult[Stream[Arc]]): Stream[Arc] = {
    result match {
      case Success(arcs, _) => arcs

      case failure => {
	Stream((Name("data:parse"), Name("data:problem"),
		Plain("@@parse failure:" + failure, None)))
      }
    }
  }
}

class NTriplesParser extends swap.ntriples.NTriplesSyntax
 with ConcreteParser
class TurtleParser(base: String) extends swap.turtle.TurtleSyntax(base)
 with ConcreteParser
class SPARQLParser(base: String) extends swap.sparql.SPARQLSyntax(base)
 with ConcreteParser



/**
 * Handy graph API
 * 
 * TODO: consider Ulman's efficient algorithm for subgraph isomorphism
 * http://en.wikipedia.org/wiki/Subgraph_isomorphism
 * http://portal.acm.org/citation.cfm?doid=321921.321925
 *
 * looked at SICP
 * http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-29.html#%_sec_4.4.4
 * didn't end up using it much.
 */
import swap.logic1.Term
import swap.logic1ec.ConjunctiveKB
import swap.rdflogic.{RDFXMLTerms, RDFLogic, Scope}

class Graph(val arcs: Iterable[(Term, Term, Term)])
extends ConjunctiveKB with RDFXMLTerms {
  override def getData(tokens: Seq[Any]) = {
    // We could/should prune the arcs that don't use these tokens,
    // but we're not bothering, for now.
    arcset.toStream.map(RDFLogic.atom _)
  }

  override def scope(taken: Iterable[Variable]): (Variable => Variable) = {
    var s = new Scope(taken)

    {
      case x: XMLVar => s.fresh(x.sym.name)
      case y => s.fresh("v")
    }
  }
     
  lazy val arcset = arcs.toSet

  /**
   * Make handy variable fresh w.r.t. all other variables in the graph.
   */
  override lazy val vars = new Scope(arcset.flatMap {
    arc => RDFLogic.variables(RDFLogic.atom(arc))
  })
  val qvar = vars.fresh("Q")

  def arcsMatching(s: Term, p: Term, o: Term): Stream[(Term, Term, Term)] = {
    val goal = RDFLogic.atom(s, p, o)
    for (solution <- solve(goal)) yield {
      val result = RDFLogic.subst(goal, solution)
      (result.args(0), result.args(1), result.args(2))
    }
  }

  /**
   * @param s: a Term. exactly 1 of s, p, o is a Variable
   * @param p: a Term. exactly 1 of s, p, o is a Variable
   * @param o: a Term. exactly 1 of s, p, o is a Variable
   *
   * @return a Stream of matching terms.
   */
  def each(s: Term, p: Term, o: Term): Stream[Term] = {
    val goal = RDFLogic.atom(s, p, o)
    assert(RDFLogic.variables(goal).size == 1)

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
    val goal = RDFLogic.atom(s, p, o)

    !solve(goal).isEmpty
  }

}

object RDFQ {
  implicit def sym(s: Symbol): Atom = Atom(s)

  def quote(term: Term): SExp = {
    term match {
      case v: XMLVar => v.sym
      case Name(n) => fromSeq(List(Symbol(n)))
      case Plain(s, None) => Atom(s)
      case Plain(s, Some(code)) => fromSeq(List('text, Atom(s), Atom(code)))
      case Data(lex, dt) => fromSeq(List('data, Atom(lex), quote(dt)))
      case XMLLit(content) => Atom(content.toString())
      case _ => Atom(Symbol("*oops*"))
    }
  }

  def quote(f: ECFormula): SExp = {
    f match {
      case Exists(vars, g) => fromSeq(List('exists,
					   fromSeq(vars.toSeq.map(quote _)),
					   quote(g)))
      case And(fmlas) => fromSeq(List('and) ++ fmlas.map(quote _))
      case Atomic(rel, args) => fromSeq(rel :: args.map(quote _))
    }
  }
}

