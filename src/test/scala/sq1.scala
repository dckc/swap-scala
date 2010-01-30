package org.w3.swap.test

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import org.w3.swap

class SparqlFragment extends Spec with ShouldMatchers {
  import swap.sparql.SPARQLParser

  describe ("parser for sparql fragment used in RDFa test suite") {
    it("should parse one case") {
      val p = new SPARQLParser("data:")

      /* taken from http://www.w3.org/2006/07/SWD/RDFa/testsuite/xhtml1-testcases-20080731/0001.sparql */
      val q0001 = """
ASK WHERE {
	<http://www.w3.org/2006/07/SWD/RDFa/testsuite/xhtml1-testcases/photo1.jpg> <http://purl.org/dc/elements/1.1/creator> "Mark Birbeck" .
}
"""

      p.parseAll(p.AskQuery, q0001) match {
	case p.Success(f, _) => {
	  println("parsed " + q0001 + " to " + f)
	  true
	}

	case p.Failure(_, rest) => {
	  println("failure: " + rest.pos.longString)
	  false
	}

	case p.Error(_, rest) => {
	  println("error: " + rest.pos.longString)
	  false
	}
      }
    }
  }
}

