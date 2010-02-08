package org.w3.swap

import org.w3.swap
import swap.logic.{Formula, Exists, And, Atomic, Term, Literal}

/**
 * WebData can read RDF in various formats.
 */
object WebData {
  import java.io.InputStreamReader
  import scala.xml.XML

  // TODO: conneg

  def loadRDFXML(addr: String): Formula = {
    import org.w3.swap.rdf.RDFXMLParser

    val e = XML.load(addr)
    val p = new RDFXMLParser(addr) // @@TODO: absolutize base
    p.parse(e)
  }

  protected def content(addr: String): InputStreamReader = {
    val conn = new java.net.URL(addr).openConnection()
    new InputStreamReader(conn.getInputStream())
  }

  /**
   * @throws IOException if openConnection(addr) throws one
   */
  def loadNT(addr: String): Formula = {
    val p = new swap.ntriples.NTriplesParser()
    p.parseAll(p.ntripleDoc, content(addr)) match {
      case p.Success(f, _) => f
      case failure => {
	throw new Exception("@@parse failure:" + failure)
      }
    }
  }

  /**
   * @throws IOException if openConnection(addr) throws one
   */
  def loadTurtle(addr: String): Formula = loadTurtle(addr, addr)

  /**
   * @throws IOException if openConnection(addr) throws one
   */
  def loadTurtle(addr: String, base: String): Formula = {
    val p = new swap.TurtleParser(base)
    p.parseAll(p.document, content(addr)) match {
      case p.Success(f, _) => f
      case failure => {
	throw new Exception("@@parse failure:" + failure)
      }
    }
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
    
  def loadRDFa(addr: String): Formula = {
    import org.w3.swap.rdf.RDFaSyntax

    // TODO: suggest media type with Accept: headers; check Content-type
    val e = XML.load(addr)
    val base = e \ "head" \ "base" \ "@href"
    RDFaSyntax.getFormula(e, if (base.isEmpty) addr else base.text)
  }

  /**
   * Absolutize a URI reference w.r.t. cwd.
   */
  def asURI(ref: String): String = {
    val cwd = java.lang.System.getProperty("user.dir")
    new java.io.File(cwd).toURI().resolve(ref).toString()
  }
}
