package org.w3.swap.sparql

import org.w3.swap
import swap.logic.Formula

import scala.util.parsing.combinator.{Parsers, RegexParsers}

/**
 * per http://www.w3.org/TR/rdf-sparql-query/#grammar
 *
 * Just enough for the RDFa test suite, i.e. ASK WHERE { pattern }
 */
class SPARQLParser(override val baseURI: String
		 ) extends swap.n3.TextRDF(baseURI) {
  import swap.logic.{Term, Formula}

  def mkstatement(s: Term, p: Term, o: Term): Formula = {
    swap.rdf.Holds(s, p, o)
  }

  def AskQuery: Parser[Formula] = (
    "(?i:ASK)".r ~> opt("(?i:WHERE)".r) ~>
    "{" ~> repsep(statement, ".") <~ opt(".") <~ "}"
    ) ^^ {
    case statements => {
      val fmlas = scopes.top.statements.toList.reverse
      mkFormula(fmlas)
    }
  }
}


