/* N-triples
 * http://www.w3.org/TR/2004/REC-rdf-testcases-20040210/#ntriples
 */

package org.w3.swap

import scala.util.parsing.combinator.{Parsers, RegexParsers}

import logicalsyntax.{Formula, And, Exists, Term}
import rdf2004.{URI,BlankNode}
import rdf2004.AbstractSyntax.{plain, data, text, atom}

class SyntaxError(msg: String) extends Exception

class NTriplesLex extends RegexParsers {
  def literal: Parser[Term] = langString | datatypeString

  /* why can't I move these methods to an object? */
  /* TODO: escapes */
  def mkuri(str: String) = new URI(dequote(str))
  def dequote(str: String) = str.substring(1, str.length() - 1)

  /* fold in absoluteURI */
  def absuri: Parser[Term] = "<[^>]+>".r ^^ {
    case str => mkuri(str)
  }

  def langString: Parser[Term] =
    /* technically, this would allow spaces around the @ */
    "\"[^\"]+\"".r ~ opt("@" ~> "[a-z]+(-[a-z0-9]+)*".r) ^^ {
      case str ~ None => plain(dequote(str))
      case str ~ Some(code) => text(dequote(str), code)
    }

  def datatypeString: Parser[Term] =
    /* technically, this would allow spaces around the ^^ */
    "\"[^\"]+\"".r ~ "^^" ~ "<[^>]+>".r ^^ {
      case value ~ _ ~ dt => data(value, mkuri(dt))
    }

}


class NTriples extends NTriplesLex {
  /* turn off whitespace skipping? or just take newlines out? */
  override val whiteSpace = "(?:(?:#.*\n)|[ \t\n]+)*".r

  def ntriplesDoc: Parser[Formula] = rep(line) ^^ {
    case List() => And(List())
    case atoms => {
      val vars = atoms.flatMap(f => f.variables)
      if (vars.isEmpty) { And(atoms) }
      else { Exists(vars.removeDuplicates, And(atoms)) }
    }
  }

  /* never mind eoln*/
  def line: Parser[Formula] = triple

  def triple: Parser[Formula] = subject ~ predicate ~ objectt <~ "." ^^ {
    case s~p~o => atom(s, p, o)
  }

  def subject: Parser[Term] = absuri | nodeID

  def predicate: Parser[Term] = absuri

  def objectt: Parser[Term] = absuri | nodeID | literal

  def nodeID: Parser[Term] = "_:[A-Za-z][A-Za-z0-9]*".r ^^ {
    case str => {
      val n = str.substring(2).intern()
      BlankNode("_:" + n, None)
    }
  }

  def toFormula(doc: String): Formula = {
    this.parseAll(ntriplesDoc, doc) match {
      case Success(f, _) => f
      case Failure(msg, _) => throw new SyntaxError(msg)
      case Error(msg, _) => throw new SyntaxError(msg)
    }
  }
}
