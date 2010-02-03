package org.w3.swap.test

import org.w3.swap
import swap.logic.{Formula, Exists, And, Atomic, Term, Literal}
import swap.rdf.{URI, BlankNode, Graph}

abstract class Namespace(n: String) {
  protected def term(name: String): URI = {
    URI(n + name)
  }
}

/**
 * per http://www.w3.org/TR/rdf-testcases/
 */
object testSchema extends Namespace(
  "http://www.w3.org/2000/10/rdf-tests/rdfcore/testSchema#") {
  final val PositiveEntailmentTest = term("PositiveEntailmentTest")
  final val NegativeEntailmentTest = term("NegativeEntailmentTest")
  final val PositiveParserTest = term("PositiveParserTest")
  final val `NT-Document` = term("NT-Document")
  final val `RDF-XML-Document` = term("RDF-XML-Document")
  final val description = term("description")
  final val status = term("status")
  final val premiseDocument = term("premiseDocument")
  final val conclusionDocument = term("conclusionDocument")
  final val entailmentRules = term("entailmentRules")
  final val simpleEntailment = term("simpleEntailment")
  final val inputDocument = term("inputDocument")
  final val outputDocument = term("outputDocument")
}

/**
 * per http://www.w3.org/TR/test-metadata/
 */
object testDescription extends Namespace(
  "http://www.w3.org/2006/03/test-description#") {
  final val TestCase = term("TestCase")
  final val informationResourceInput = term("informationResourceInput")
  final val informationResourceResults = term("informationResourceResults")
  final val reviewStatus = term("reviewStatus")
  final val approved = term("approved")
  final val purpose = term("purpose")
}

object dc extends Namespace(
  "http://purl.org/dc/elements/1.1/") {
  final val title = term("title")
}

sealed abstract class TestResult
case class RunResult(pass: Boolean) extends TestResult
case class BadTestData(msg: String) extends TestResult
case class UnsupportedFeature(msg: String) extends TestResult

abstract class TestSuite(val manifest: Graph) {
  def run(): Stream[(Term, String, TestResult)]
}

class EntailmentTestSuite(override val manifest: Graph)
extends TestSuite(manifest) {
  import swap.rdf.AbstractSyntax.{conjunction, wellformed, rdf_type, plain }
  import swap.rdf.Semantics.entails

  val what = manifest.qvar
  val what2 = what.fresh()
  
  override def run(): Stream[(Term, String, TestResult)] = {
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
	    case testSchema.PositiveParserTest => {
	      (test, desc, positiveParserTest(test))
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

  def positiveParserTest(test: Term): TestResult = {
    val fin = load(manifest.any(test, testSchema.inputDocument, what))
    val fout = load(manifest.any(test, testSchema.outputDocument, what))

    RunResult(entails(fin, fout) && entails(fout, fin))
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

class RDFaTestSuite(override val manifest: Graph) extends TestSuite(manifest) {
  import swap.rdf.AbstractSyntax.{rdf_type}
  import swap.rdf.Semantics.entails

  val what = manifest.qvar
  val what2 = what.fresh()
  
  override def run(): Stream[(Term, String, TestResult)] = {
    for {
      test <- manifest.each(what, rdf_type, testDescription.TestCase)
      title = manifest.any(test, dc.title, what)
      indoc = manifest.any(test,
			   testDescription.informationResourceInput,
			   what)
      outq = manifest.any(test,
			  testDescription.informationResourceResults,
			  what)
    } yield {
      (title, indoc, outq) match {
	case (Literal(titlestr: String), inaddr: URI, outaddr: URI) =>
	  if (!manifest.contains(test, testDescription.reviewStatus,
				 testDescription.approved))
 	    (test, titlestr, BadTestData("test not approved"))
	  else {
	    val data = WebData.loadRDFa(inaddr.i)
	    val pattern = WebData.loadSPARQL(outaddr.i)

	    println("@@rdfa parser vs sparqlq")
	    println(data)
	    println(pattern)
	    val result = entails(data, pattern)

	    (test, titlestr, RunResult(result))
	  }
	case _ => (test, "?", BadTestData("non-Literal description"))
      }
    }
  }
}

class RDFaExample(indoc: String, outdoc: String) {
  import swap.rdf.Semantics.entails
  import swap.rdf.RDFaParser

  def run(): Stream[(Term, String, TestResult)] = {
    val base = WebData.asURI(indoc)
    val actual = WebData.loadRDFa(base)
    val expected = WebData.loadTurtle(WebData.asURI(outdoc), base)
    val aTest = swap.rdf.BlankNode("test", Some(this.hashCode()))

    val result = entails(actual, expected) && entails(expected, actual)
    Stream.cons((aTest, indoc, RunResult(result)),
		Stream.empty)
  }
}

object Runner {
  def main(args: Array[String]): Unit = {
    val manifest = new Graph(WebData.loadRDFXML(args(1)))

    val results = args(0) match {
      case "--entailment" => new EntailmentTestSuite(manifest).run()
      case "--rdfa" => new RDFaTestSuite(manifest).run()
      case "--rdfax" => new RDFaExample(args(1), args(2)).run()
      case _ => {
	println("Usage: rdfstdtest --entailment|--rdfa manifest")
	Stream.empty
      }
    }

    for ((test, desc, result) <- results) {
      println()
      println ("=== ")
      println()
      println("" + result + test)
      println(desc)
    }
  }
}

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
	// TODO: throw exception
	println("SPARQL @@parse failure:")
	println(failure)
	And(Nil)
      }
    }
  }
    
  def loadRDFa(addr: String): Formula = {
    import org.w3.swap.rdf.RDFaParser

    val e = XML.load(addr)
    val p = new RDFaParser(addr) // @@TODO: absolutize base?
    p.parse(e)
  }

  /**
   * Convert a (local) file path to a URI.
   */
  def asURI(fp: String): String = {
    val cwd = java.lang.System.getProperty("user.dir")
    new java.io.File(cwd, fp).toURI().toString()
  }
}
