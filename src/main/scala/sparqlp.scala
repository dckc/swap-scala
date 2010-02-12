package org.w3.swap.sparql

import scala.util.parsing.combinator.{Parsers, RegexParsers}

import org.w3.swap

/**
 * per http://www.w3.org/TR/rdf-sparql-query/#grammar
 *
 * Just enough for the RDFa test suite, i.e. ASK WHERE { pattern }
 */
abstract class SPARQLSyntax(override val initialBase: String
		 ) extends swap.turtle.TurtleSyntax(initialBase) {

  def AskQuery: Parser[Stream[Arc]] = (
    "(?i:ASK)".r ~> opt("(?i:WHERE)".r) ~>
    "{" ~> repsep(statement, ".") <~ opt(".") <~ "}" ^^ {
      case statements => statements.toStream.flatMap { case arcs => arcs }
    }
  )
}


