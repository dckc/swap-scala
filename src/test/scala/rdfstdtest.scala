package org.w3.swap.test

import org.w3.swap
import swap.webdata.WebData
import swap.logic1.Term
import swap.logic1ec.ECFormula
import swap.rdflogic.{RDFXMLTerms, Name, Plain}
import swap.webdata.Graph

abstract class Namespace(n: String) {
  protected def term(name: String): Name = {
    Name(n + name)
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

abstract class TestSuite(val manifest: Graph) extends RDFXMLTerms {
  def run(): Stream[(Term, String, TestResult)]
}

class EntailmentTestSuite(override val manifest: Graph)
extends TestSuite(manifest) {
  import swap.logic1ec.And
  import swap.rdflogic.{RDFLogic => RL }

  val what = manifest.qvar
  val what2 = manifest.vars.fresh("what")
  
  override def run(): Stream[(Term, String, TestResult)] = {
    for {
      (test, td, o) <- manifest.arcsMatching(what, testSchema.description,
					     what2)
    } yield {
      o match {
	case Plain(desc, _) => {
	  if (!manifest.contains(test, testSchema.status,
				 Plain("APPROVED", None)))
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
      case Name(i) => i == nsuri
      case _ => false
    }
  }

  def entailmentTest(test: Term, expected: Boolean): TestResult = {
    if (!isSimple(test))
      UnsupportedFeature("entailment rules beyond simple")
    else {
      val premises = manifest.each(test, testSchema.premiseDocument, what
				 ).map(t => load(t))
      val premise = premises.foldLeft(And(Nil):ECFormula)(
	(f, g) => RL.conjunction(f, g))

      val conclusion = load(manifest.any(test, testSchema.conclusionDocument,
					 what))
      println()
      println(test)
      println("premise: " + premise)
      println("entails?" + expected)
      println("conclusion: " + conclusion)

      RunResult(RL.entails(premise, conclusion) == expected)
    }
  }

  def positiveParserTest(test: Term): TestResult = {
    val fin = load(manifest.any(test, testSchema.inputDocument, what))
    val fout = load(manifest.any(test, testSchema.outputDocument, what))

    RunResult(RL.entails(fin, fout) && RL.entails(fout, fin))
  }

  def load(u: Term): ECFormula = {
    val arcs = u match {
      case Name(addr) => {
	if (manifest.contains(u, rdf_type,
			      testSchema.`RDF-XML-Document`) ) {
	  WebData.loadRDFXML(addr)
	} else if (manifest.contains(u, rdf_type, testSchema.`NT-Document`)) {
	  WebData.loadNT(addr)
	} else {
	  println("@@unknown document type:  for test document " + u)
	  println(manifest.each(u, rdf_type, what).mkString("types:",
							      "\n ", "\n"))
	  Stream.empty
	}
      }
      case _ => {
	println("@@document term not a URI: " + u)
	println("@@TODO FalseDocument support.")
	Stream.empty
      }
    }
    RL.graphFormula(arcs)
  }
}

/* @@ need to restore SPARQL parser...
class RDFaTestSuite(override val manifest: Graph) extends TestSuite(manifest) {
  import swap.rdflogic.{RDFLogic => RL}

  val what = manifest.qvar
  val what2 = manifest.vars.fresh("what")
  
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
	case (Plain(titlestr, _), Name(inaddr), Name(outaddr)) =>
	  if (!manifest.contains(test, testDescription.reviewStatus,
				 testDescription.approved))
 	    (test, titlestr, BadTestData("test not approved"))
	  else {
	    val data = RL.graphFormula(WebData.loadRDFa(inaddr))
	    val pattern = RL.graphFormula(WebData.loadSPARQL(outaddr))

	    if (pattern == null) {
	      (test, titlestr, UnsupportedFeature("SPARQL parse failure"))
	    } else {
	      // TODO: handle expectedResults false
	      val result = RL.entails(data, pattern)
	      if (!result) {
		println()
		println("expected (from SPARQL):")
		println(pattern.quote().pretty())
		println("actual:")
		println(data.quote().pretty())
	      }
	      (test, titlestr, RunResult(result))
	    }
	  }
	case _ => (test, "?", BadTestData("non-Literal description"))
      }
    }
  }
}
*/

class RDFaExample(indoc: String, outdoc: String) {
  import swap.rdflogic.{RDFLogic => RL}
  import swap.webdata.RDFQ

  def run(): Stream[(Term, String, TestResult)] = {
    val base = WebData.cwdbased(indoc)
    val actual = RL.graphFormula(WebData.loadRDFa(base))
    val expected = RL.graphFormula(
      WebData.loadTurtle(WebData.cwdbased(outdoc), base))
    val aTest = swap.rdflogic.XMLVar("test", Some(this.hashCode()))

    val result = RL.entails(actual, expected) && RL.entails(expected, actual)

    if(!result) {
      println("expected: " + RDFQ.quote(expected).pretty())
      println("actual: " + RDFQ.quote(actual).pretty())
    }

    Stream.cons((aTest, indoc, RunResult(result)),
		Stream.empty)
  }
}

object Runner {
  def main(args: Array[String]): Unit = {
    lazy val manifest = new Graph(WebData.loadRDFXML(args(1)))

    val results = args(0) match {
      case "--entailment" => new EntailmentTestSuite(manifest).run()
//@@      case "--rdfa" => new RDFaTestSuite(manifest).run()
      case "--rdfax" => new RDFaExample(args(1), args(2)).run()
      case _ => {
	println("Usage: rdfstdtest --entailment|--rdfa manifest")
	Stream.empty
      }
    }

    for ((test, desc, result) <- results) {
      println()
      println("" + result + desc)
      println("" + test)
      println()
      println ("=== ")
    }
  }
}

