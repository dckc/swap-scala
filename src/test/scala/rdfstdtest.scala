package org.w3.swap.test

import org.w3.swap
import swap.webdata.{WebData, URLOpener}
import swap.logic1.Term
import swap.logic1ec.ECFormula
import swap.rdflogic.{RDFXMLTerms, Name, Plain, XMLVar, Scope}
import swap.webdata.Graph
import swap.rdfxml
import swap.rdfxml.{SimpleSerializer => RDFout}

abstract class Namespace(n: String) {
  protected def term(name: String): Name = {
    Name(n + name)
  }
}

object RDFS extends Namespace("http://www.w3.org/2000/01/rdf-schema#") {
  final val label = term("label")
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

object DC extends Namespace(
  "http://purl.org/dc/elements/1.1/") {
  final val title = term("title")
  final val description = term("description")
}

object FOAF extends Namespace("http://xmlns.com/foaf/0.1/") {
  final val homepage = term("homepage")
}

object DOAP extends Namespace("http://usefulinc.com/ns/doap#") {
  final val name = term("name")
}

object EARL extends Namespace("http://www.w3.org/ns/earl#") {
  final val subject = term("subject")
  final val test = term("test")
  final val outcome = term("outcome")
  final val pass = term("pass")
  final val fail = term("fail")
  final val result = term("result")
}

sealed abstract class TestResult
case class RunResult(pass: Boolean) extends TestResult
case class BadTestData(msg: String) extends TestResult
case class UnsupportedFeature(msg: String) extends TestResult

abstract class TestSuite(val manifest: Graph) extends RDFXMLTerms {
  def run(): Stream[(Term, String, TestResult)]
}

class EntailmentTestSuite(override val manifest: Graph, web: URLOpener)
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
	    /* TODO: finish RDF/XML parser
	    case testSchema.PositiveParserTest => {
	      (test, desc, positiveParserTest(test))
	    }
	    */
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
      val actual = RL.entails(premise, conclusion)

      if (actual != expected) {
	DEBUG("")
	DEBUG(test)
	DEBUG("premise: " + premise)
	DEBUG("entails?" + expected)
	DEBUG("conclusion: " + conclusion)
      }

      RunResult(actual == expected)
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
	  WebData.loadData(web, addr, WebData.RDFXML)
	} else if (manifest.contains(u, rdf_type, testSchema.`NT-Document`)) {
	  WebData.loadNT(web, addr)
	} else {
	  DEBUG("@@unknown document type:  for test document " + u)
	  DEBUG(manifest.each(u, rdf_type, what).mkString("types:",
							      "\n ", "\n"))
	  Stream.empty
	}
      }
      case _ => {
	DEBUG("@@document term not a URI: " + u)
	DEBUG("@@TODO FalseDocument support.")
	Stream.empty
      }
    }
    RL.graphFormula(arcs)
  }
}

class RDFaTestSuite(override val manifest: Graph, web: URLOpener)
extends TestSuite(manifest) {
  import swap.rdflogic.{RDFLogic => RL}
  import swap.webdata.RDFQ

  val what = manifest.qvar
  val what2 = manifest.vars.fresh("what")
  
  override def run(): Stream[(Term, String, TestResult)] = {
    for {
      test <- manifest.each(what, rdf_type, testDescription.TestCase)
      title = manifest.any(test, DC.title, what)
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
	    val data = RL.graphFormula(WebData.loadData(web, inaddr,
							WebData.RDFa_types))
	    val pattern = RL.graphFormula(WebData.loadSPARQL(web,
							     outaddr,
							     outaddr))

	    if (pattern == null) {
	      (test, titlestr, UnsupportedFeature("SPARQL parse failure"))
	    } else {
	      // TODO: handle expectedResults false
	      val result = RL.entails(data, pattern)
	      if (!result) {
		DEBUG()
		DEBUG("expected (from SPARQL):")
		DEBUG(RDFQ.quote(pattern).pretty())
		DEBUG("actual:")
		DEBUG(RDFQ.quote(data).pretty())
	      }
	      (test, titlestr, RunResult(result))
	    }
	  }
	case _ => (test, "?", BadTestData("non-Literal description"))
      }
    }
  }
}

class RDFaExample(indoc: String, outdoc: String) {
  import swap.rdflogic.{RDFLogic => RL}
  import swap.webdata.RDFQ

  def run(): Stream[(Term, String, TestResult)] = {
    val web = new URLOpener()
    val base = WebData.cwdbased(indoc)
    val actual = RL.graphFormula(WebData.loadData(web, base,
						  WebData.RDFa_types))
    val expected = RL.graphFormula(
      WebData.loadTurtle(web, WebData.cwdbased(outdoc), base))
    val aTest = swap.rdflogic.XMLVar("test", Some(this.hashCode()))

    val result = RL.entails(actual, expected) && RL.entails(expected, actual)

    if(!result) {
      DEBUG("expected: " + RDFQ.quote(expected).pretty())
      DEBUG("actual: " + RDFQ.quote(actual).pretty())
    }

    Stream.cons((aTest, indoc, RunResult(result)),
		Stream.empty)
  }
}

object Runner {
  def main(args: Array[String]): Unit = {
    lazy val mirror = new MirrorOpener(WebData.cwdbased(args(1)),
				       WebData.cwdbased(args(2)))
    lazy val manifest = new Graph(WebData.loadData(mirror, args(1),
						   WebData.RDFXML))

    val results = args(0) match {
      case "--entailment" => new EntailmentTestSuite(manifest, mirror).run()
      case "--rdfa" => new RDFaTestSuite(manifest, mirror).run()
      case "--rdfax" => new RDFaExample(args(1), args(2)).run()
      case _ => {
	System.err.println(
	  "Usage: rdfstdtest --entailment|--rdfa manifest mirror")
	Stream.empty
      }
    }

    val stdout = new java.io.OutputStreamWriter(java.lang.System.out)

    import swap.rdflogic.{RDFLogic => RL }
    val fresh = new Scope(Nil).fresh _
    val sw = fresh("sw")
    val swdesc = Stream[RDFout.Arc](
      (sw, DOAP.name, Plain("swap-scala", None)),
      (sw, FOAF.homepage, Name("http://code.google.com/p/swap-scala/"))
    )
    val testarcs = results.toStream.flatMap { case (test, desc, result) =>
      val assertion = fresh("assertion")
      val earlresult = fresh("result")
      val resultdesc = result match {
	case RunResult(true) => Stream((earlresult, EARL.outcome, EARL.pass))
	case RunResult(false) => Stream((earlresult, EARL.outcome, EARL.fail))
	case BadTestData(msg) => {
	  val outcome = fresh("badt")
	  Stream((earlresult, EARL.outcome, outcome),
		 (outcome, RDFS.label, Plain(msg, None)))
	}
	case UnsupportedFeature(msg) => {
	  val outcome = fresh("unsup")
	  Stream((earlresult, EARL.outcome, outcome),
		 (outcome, RDFS.label, Plain(msg, None)))
	}
      }
      resultdesc ++ Stream(
	(test, DC.description, Plain(desc, None)),
	(assertion, EARL.subject, sw),
	(assertion, EARL.test, test),
	(assertion, EARL.result, earlresult) )
    }

    RDFout.writeArcsDoc(stdout, swdesc ++ testarcs)
    stdout.close()
  }
}

/**
 * Load resources from a local copy.
 * @param global: URI of original resource
 * @param local: URI of mirror of global
 */
class MirrorOpener(val global: String, val local: String)
extends URLOpener{
  import java.net.{URI, URL, URLConnection}
  import java.io.InputStreamReader

  protected val gu = new URI(global).resolve("./")
  protected val lu = new URI(local).resolve("./")

  override def open(addr: String,
		    accept: String): (InputStreamReader, URLConnection) = {
    val actual = lu.resolve(gu.relativize(new URI(addr)))

    val conn = actual.toURL.openConnection()
    conn.setRequestProperty("accept", accept)
    val reader = new InputStreamReader(conn.getInputStream())
    (reader, conn)
  }

}

object DEBUG{
  def apply(s: Any) = System.err.println(s.toString)
}
