package org.w3.swap.test

import org.w3.swap
import swap.logic.{Formula, Exists, And, Atomic, Term, Literal}
import swap.rdf.{URI, BlankNode, Graph}

object testSchema {

  protected def term(name: String): URI = {
    URI("http://www.w3.org/2000/10/rdf-tests/rdfcore/testSchema#" + name)
  }

  final val PositiveEntailmentTest = term("PositiveEntailmentTest")
  final val NegativeEntailmentTest = term("NegativeEntailmentTest")
  final val `NT-Document` = term("NT-Document")
  final val `RDF-XML-Document` = term("RDF-XML-Document")
  final val description = term("description")
  final val status = term("status")
  final val premiseDocument = term("premiseDocument")
  final val conclusionDocument = term("conclusionDocument")
  final val entailmentRules = term("entailmentRules")
  final val simpleEntailment = term("simpleEntailment")
}

sealed abstract class TestResult
case class RunResult(pass: Boolean) extends TestResult
case class BadTestData(msg: String) extends TestResult
case class UnsupportedFeature(msg: String) extends TestResult

class EntailmentTestSuite(val manifest: Graph) {
  import swap.rdf.AbstractSyntax.{conjunction, wellformed, rdf_type, plain }
  import swap.rdf.Semantics.entails

  val what = manifest.qvar
  val what2 = what.fresh()

  def run(): Stream[(Term, String, TestResult)] = {
    for {
      (test, td, o) <- manifest.arcsMatching(what, testSchema.description,
					     what2)
    } yield {
      o match {
	case Literal(desc: String) => {
	  if (!manifest.contains(test, testSchema.status,
				 Literal("APPROVED")))
 	    (test, desc, BadTestData("test not APPROVED"))
	  else manifest.any(test, rdf_type, what) match {
	    case testSchema.PositiveEntailmentTest => {
	      (test, desc, entailmentTest(test, true))
	    }
	    case testSchema.NegativeEntailmentTest => {
	      (test, desc, entailmentTest(test, false))
	    }
	    case t => {
	      (test, desc, UnsupportedFeature("test type: " + t))
	    }
	  }
	}
	case _ => (test, "?", BadTestData("non-Literal description"))
      }
    }
  }

  def isSimple(test: Term): Boolean = {
    import swap.rdf.Vocabulary.nsuri

    val rules = manifest.each(test, testSchema.entailmentRules, what)

    rules.forall {
      case testSchema.simpleEntailment => true
      case URI(i) => i == nsuri
      case _ => false
    }
  }

  def entailmentTest(test: Term, expected: Boolean): TestResult = {
    if (!isSimple(test))
      UnsupportedFeature("entailment rules beyond simple")
    else {
      val premises = manifest.each(test, testSchema.premiseDocument, what
				 ).map(t => load(t))
      val premise = premises.foldLeft(And(Nil):Formula)(
	(f, g) => conjunction(f, g))

      val conclusion = load(manifest.any(test, testSchema.conclusionDocument,
					 what))
      println()
      println(test)
      println("premise: " + premise)
      println("entails?" + expected)
      println("conclusion: " + conclusion)

      RunResult(entails(premise, conclusion) == expected)
    }
  }

  def load(u: Term): Formula = {
    u match {
      case URI(addr) => {
	if (manifest.contains(u, rdf_type,
			      testSchema.`RDF-XML-Document`) ) {
	  WebData.loadRDFXML(addr)
	} else if (manifest.contains(u, rdf_type, testSchema.`NT-Document`)) {
	  WebData.loadNT(addr)
	} else {
	  println("@@unknown document type:  for test document " + u)
	  println(manifest.each(u, rdf_type, what).mkString("types:",
							      "\n ", "\n"))
	  And(Nil) // or use Forall(), i.e. non-RDF?
	}
      }
      case _ => {
	println("@@document term not a URI: " + u)
	println("@@TODO FalseDocument support.")
	And(Nil)
      }
    }
  }
}


object Runner {
  def main(args: Array[String]): Unit = {
    val manifest = new Graph(WebData.loadRDFXML(args(0)))

    for ((test, desc, result) <- new EntailmentTestSuite(manifest).run()) {
      println()
      println ("=== ")
      println()
      println(test)
      println(desc)
      println(result)
    }
  }

}

object WebData {
  // TODO: conneg

  def loadRDFXML(addr: String): Formula = {
    import scala.xml.XML
    import org.w3.swap.rdf.RDFXMLParser

    val e = XML.load(addr)
    val p = new RDFXMLParser(addr) // @@TODO: absolutize base
    p.parse(e)
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
}

    
