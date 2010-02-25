package org.w3.swap.test

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import org.w3.swap

class SparqlFragment extends Spec with ShouldMatchers {
  import swap.webdata.SPARQLParser

  describe ("parser for sparql fragment used in RDFa test suite") {
    it("should parse one case") {
      val p = new SPARQLParser("data:")

      /* taken from http://www.w3.org/2006/07/SWD/RDFa/testsuite/xhtml1-testcases-20080731/0057.sparql */
      val q0001 = """ASK WHERE {
	<http://www.example.org/#ben> <http://xmlns.com/foaf/0.1/knows> <http://www.example.org/#mark> .
	<http://www.example.org/#ben> <http://xmlns.com/foaf/0.1/knows> <http://www.example.org/#ivan> .
	<http://www.example.org/#mark> <http://xmlns.com/foaf/0.1/name> "Mark Birbeck" .
	<http://www.example.org/#ivan> <http://xmlns.com/foaf/0.1/name> "Ivan Herman" .
}"""

      p.parseAll(p.AskQuery, q0001) match {
	case p.Success(f, _) => {
	  // println("parsed " + q0001 + " to " + f)
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

