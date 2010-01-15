package org.w3.swap.test

import org.w3.swap
import swap.logic.{Formula, Exists, And, Atomic, Term, Literal}
import swap.rdf.{URI, BlankNode, NotNil}

object testSchema {

  protected def term(name: String): URI = {
    URI("http://www.w3.org/2000/10/rdf-tests/rdfcore/testSchema#" + name)
  }

  final val PositiveEntailmentTest = term("PositiveEntailmentTest")
  final val NegativeEntailmentTest = term("NegativeEntailmentTest")
  final val `NT-Document` = term("NT-Document")
  final val `RDF-XML-Document` = term("RDF-XML-Document")
  final val description = term("description")
  final val premiseDocument = term("premiseDocument")
  final val conclusionDocument = term("conclusionDocument")
}

class EntailmentTestSuite(val manifest: Formula) {
  import swap.rdf.AbstractSyntax.{wellformed, rdf_type, atom, plain }
  import swap.rdf.Semantics.{conjoin, entails}
  import swap.rdf.KnowledgeBase

  /**
   * @param f Formula from which to match
   * @param goal: a NotNil with 1 variable
   *
   * @return a list of matching terms. hmm... List? Seq? Stream? Iterable?
   *         unless f is RDF-well-formed, will return empty
   */
  def each(f: Formula, goal: NotNil): List[Term] = {
    assert(goal.variables.toList.length == 1)

    val atoms: Iterable[Formula] = f match {
      case x: Atomic => List(f)
      case And(l) => l
      case Exists(vl, And(l)) => l
      case _ => Nil // ill-formed
    }

    // TODO: re-think this Stream stuff.
    for(subst <- new KnowledgeBase(atoms.toStream).solve(goal).toList) yield {
      // value of 1st
      subst.valuesIterator.next
    }
  }

  val q = BlankNode("_:q", None)

  def run(): List[(Term, String, Boolean)] = {
    for(test <- each(manifest,
		     atom(q, rdf_type, testSchema.PositiveEntailmentTest)))
      yield {
	val (desc, passed) = run1(test)
	(test, desc, passed)
      }
  }

  def load(u: Term): Formula = {
    u match {
      case URI(addr) => {
	val utype = each(manifest, atom(u, rdf_type, q)).head

	utype match {
	  // hmm... eq vs equal in match...
	  case URI(testSchema.`RDF-XML-Document`.i) => Runner.webdata(addr)
	  case URI(testSchema.`NT-Document`.i) => loadNT(addr)
	  case _ => {
	    println("@@unkown document type: " + utype +
		    " for test document " + u)
	    And(Nil)
	  }
	}
      }
      case _ => {
	println("@@document term not a URI: " + u)
	And(Nil)
      }
    }
  }

  /**
   * @throws IOException if openConnection(addr) throws one
   */
  def loadNT(addr: String): Formula = {
    import java.io.InputStreamReader
    import swap.ntriples.NTriplesParser
    
    val conn = new java.net.URL(addr).openConnection()
    val in = new InputStreamReader(conn.getInputStream())

    val p = new NTriplesParser()
    p.parseAll(p.ntripleDoc, in) match {
      case p.Success(f, _) => f
      case failure => {
	println("@@parse failure:")
	println(failure)
	And(Nil)
      }
    }
  }

    
  def run1(test: Term): (String, Boolean) = {
    val description = each(manifest, atom(test, testSchema.description, q)
			 ).head match {
      case Literal(s: String) => s
      case _ => ""
    }
    val premises = each(manifest, atom(test, testSchema.premiseDocument, q)
		      ).map(t => load(t))
    val premise = premises.foldLeft(And(Nil):Formula)((f, g) => conjoin(f, g))

    val conclusion = load(each(manifest,
			       atom(test, testSchema.premiseDocument, q)).head)

    (description, entails(premise, conclusion))
  }
}


object Runner {
  def main(args: Array[String]): Unit = {
    val manifest = webdata(args(0))

    for ((test, desc, result) <- new EntailmentTestSuite(manifest).run())
      println (result, desc, test)
  }

  def webdata(addr: String): Formula = {
    import scala.xml.XML
    import org.w3.swap.rdf.RDFXMLParser

    val e = XML.load(addr)
    val p = new RDFXMLParser(addr) // @@TODO: absolutize base
    p.parse(e)
  }
}
