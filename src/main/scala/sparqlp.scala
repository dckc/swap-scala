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
  import swap.n3.NotNil
  import swap.logic.Apply
  import swap.rdf.Holds

  def AskQuery: Parser[Formula] = (
    "(?i:ASK)".r ~> opt("(?i:WHERE)".r) ~>
    "{" ~> repsep(statement, ".") <~ opt(".") <~ "}"
    ) ^^ {
    case statements => {
      val n3fmlas = scopes.top.statements.toList.reverse
      val rdffmlas = n3fmlas.map {
	case NotNil(Apply('holds, List(s, p, o))) => Holds(s, p, o)
	case f => throw new Exception("goofy atom from N3 parser" + f)
      }
      mkFormula(rdffmlas)
    }
  }
}


