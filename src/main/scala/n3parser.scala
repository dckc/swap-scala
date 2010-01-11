package org.w3.swap

/*
 * based on
 * http://www.w.w3org/2000/10/swap/grammar/notation3.bnf
 * 1.14 2006/06/22 22:03:21
 */

import scala.util.parsing.combinator.{Parsers, RegexParsers}

import logicalsyntax.{Formula, And, Exists, Term}
import rdf2004.{URI,BlankNode}
import rdf2004.AbstractSyntax.{plain, data, text, atom}

class N3Lex extends RegexParsers {
  import java.math.BigDecimal

  val integer: Parser[Int] = "[+-]?[0-9]+".r ^^ {
    case numeral => {
      val n = if (numeral.startsWith("+")) numeral.substring(1) else numeral
      java.lang.Integer.parseInt(n)
    }
  }

  val double: Parser[Double] = "[+-]?[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)".r ^^ {
    case numeral => java.lang.Double.parseDouble(numeral)
  }

  val decimal: Parser[BigDecimal] = "[+-]?[0-9]+(\\.[0-9]+)".r ^^ {
    case numeral => new BigDecimal(numeral)
  }

  // TODO: more tokens
}

class N3Parser extends RegexParsers {
  // TODO: N3Parser
}

