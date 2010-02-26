package org.w3.swap.test

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import org.w3.swap

class RIFMisc extends Spec with ShouldMatchers {
  import swap.logic1n3.RIFParser

  describe("N3Logic RIF XML reader") {

    val l = new TestN3Logic(Nil)

    it("should handle one RIF test input") {
      val addr = "http://www.w3.org/2005/rules/test/repository/tc/RDF_Combination_Member_1/RDF_Combination_Member_1-premise.rif"
      val e = scala.xml.XML.load(addr)
      val fmla = RIFParser.rif_document(e, addr)

      // TODO: n3 serializer? pretty-printer?
      l.quote(fmla).pretty().toString should equal (
"""(notnil
  (AND
    (related
      (http://example.org/example#i )
      (http://www.w3.org/1999/02/22-rdf-syntax-ns#type )
      (http://example.org/example#A )
      )
    (AND
      (related
        (http://example.org/example#A )
        (http://www.w3.org/2000/01/rdf-schema#subClassOf )
        (http://example.org/example#B )
        )
      (related
        (http://example.org/example#k )
        (http://www.w3.org/1999/02/22-rdf-syntax-ns#type )
        (http://example.org/example#A )
        )
      )
    )
  )""")
    }
  }
}
