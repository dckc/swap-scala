package org.w3.swap.test

import org.w3.swap
import swap.logic.{Formula, Exists, And, Atomic, Term}

object testSchema {
  import swap.rdf.URI

  protected def term(name: String): Term = {
    URI("http://www.w3.org/2000/10/rdf-tests/rdfcore/testSchema#" + name)
  }

  final val PositiveEntailmentTest = term("PositiveEntailmentTest")
  final val NegativeEntailmentTest = term("NegativeEntailmentTest")
  final val `NT-Document` = term("NT-Document")
  final val `RDF-XML-Document` = term("RDF-XML-Document")
}

class EntailmentTestSuite(val manifest: Formula) {
  import swap.rdf.{NotNil, BlankNode}
  import swap.rdf.AbstractSyntax.{wellformed, rdf_type, atom, plain }
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

  def run(): List[(Term, Term)] = {
    for(test <- each(manifest,
		     atom(BlankNode("_:test", None),
			  rdf_type,
			  testSchema.PositiveEntailmentTest)))
      yield (test, plain("@@TODO: run this test"))
  }
}


object Runner {
  def main(args: Array[String]): Unit = {
    val manifest = webdata(args(0))

    for ((test, result) <- new EntailmentTestSuite(manifest).run())
      println (test, result)
  }

  def webdata(addr: String): Formula = {
    import scala.xml.XML
    import org.w3.swap.rdf.RDFXMLParser

    val e = XML.load(addr)
    val p = new RDFXMLParser(addr) // @@TODO: absolutize base
    p.parse(e)
  }
}
