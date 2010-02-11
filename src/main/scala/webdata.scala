package org.w3.swap.webdata

import java.io.InputStreamReader
import scala.xml.XML
import scala.util.parsing.combinator.Parsers

import org.w3.swap
import swap.rdfxml
import swap.ntriples
import swap.rdfa
import swap.rdflogic.TermNode

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
//@@
  def loadTurtle(addr: String): Stream[Arc] = loadTurtle(addr, addr)
*/

  /**
   * @throws IOException if openConnection(addr) throws one
  def loadTurtle(addr: String, base: String): Stream[Arc] = {
    val p = new TurtleParser(base)
    p.arcs(p.parseAll(p.document, content(addr)))
  }

  def loadSPARQL(addr: String): Formula = {
    val p = new swap.sparql.SPARQLParser(addr)

    p.parseAll(p.AskQuery, content(addr)) match {
      case p.Success(f, _) => f
      case failure => {
	println("SPARQL @@parse failure:")
	println(failure)
	null
      }
    }
  }
*/
    
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

object XMLtoRDFlogic extends rdfxml.XMLtoRDF with TermNode {
  type BlankNode = rdfxml.XMLVar

  lazy val vars = new rdfxml.Scope()
  def fresh(hint: String) = vars.fresh(hint)
  def byName(name: String) = vars.byName(name)
}

object RDFaParser extends rdfa.RDFaSyntax with TermNode {
  type BlankNode = rdfxml.XMLVar

  lazy val vars = new rdfxml.Scope()
  def fresh(hint: String) = vars.fresh(hint)
  def byName(name: String) = vars.byName(name)
}

trait ConcreteParser extends Parsers with TermNode {
  type BlankNode = rdfxml.XMLVar

  lazy val scope = new rdfxml.Scope()
  def blankNode(n: String) = scope.byName(n)

  def arcs(result: this.ParseResult[Stream[Arc]]): Stream[Arc] = {
    result match {
      case Success(arcs, _) => arcs

      case failure => {
	throw new RuntimeException("@@parse failure:" + failure)
      }
    }
  }
}

class NTriplesParser extends ntriples.NTriplesSyntax with ConcreteParser
//TODO class TurtleParser extends TurtleSyntax with ConcreteParser
