package org.w3.swap.sparql

import org.w3.swap
import swap.logic.Formula
import swap.n3.N3Parser

import scala.util.parsing.combinator.{Parsers, RegexParsers}

/**
 * per http://www.w3.org/TR/rdf-sparql-query/#grammar
 *
 * Just enough for the RDFa test suite, i.e. ASK WHERE { pattern }
 */
class SPARQLParser(override val baseURI: String
		 ) extends N3Parser(baseURI) {

  def AskQuery: Parser[Formula] = (
    "(?i:ASK)".r ~> opt("(?i:WHERE)".r) ~>
    "{" ~> repsep(statement, ".") <~ opt(".") <~ "}"
    ) ^^ {
    case statements => mkFormula(scopes.top.statements.toList.reverse)
  }
}


