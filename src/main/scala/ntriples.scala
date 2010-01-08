/* N-triples
 * http://www.w3.org/TR/2004/REC-rdf-testcases-20040210/#ntriples
 */

package org.w3.swap

import scala.util.parsing.combinator.{Parsers, RegexParsers}

import logicalsyntax.{Formula, And, Exists, Term, FunctionSymbol}
import rdf2004.{URI,
		BlankNode,
		PlainLiteral, DatatypedLiteral,
		Text, Language,
		AbstractSyntax
	      }

class SyntaxError(msg: String) extends Exception

class NTriples extends RegexParsers {
  /* turn off whitespace skipping? or just take newlines out? */
  override val whiteSpace = "(?:(?:#.*\n)|[ \t\n]+)*".r

  def ntriplesDoc: Parser[Formula] = rep(line) ^^ {
    case List() => And(List())
    case List(f) => {
      val vars = f.variables
      if (vars.isEmpty) { f }
      else { Exists(vars, f) }
    }
    case atoms => {
      val vars = atoms.flatMap(f => f.variables).removeDuplicates
      if (vars.isEmpty) { And(atoms) }
      else { Exists(vars, And(atoms)) }
    }
  }

  /* never mind eoln*/
  def line: Parser[Formula] = triple

  def triple: Parser[Formula] = subject ~ predicate ~ objectt <~ "." ^^ {
    case s~p~o => AbstractSyntax.triple(s, p, o)
  }

  def subject: Parser[Term] = uriref | nodeID

  def predicate: Parser[Term] = uriref

  def objectt: Parser[Term] = uriref | nodeID | literal

  /* fold in absoluteURI */
  def uriref: Parser[Term] = "<[^>]+>".r ^^ {
    case str => mkuri(str)
  }

  def nodeID: Parser[Term] = "_:[A-Za-z][A-Za-z0-9]*".r ^^ {
    case str => {
      val n = str.substring(2).intern()
      BlankNode("ID", n)
    }
  }

  def literal: Parser[Term] = langString | datatypeString

  def langString: Parser[Term] =
    "\"[^\"]+\"".r ~ opt("@" ~> "[a-z]+(-[a-z0-9]+)*".r) ^^ {
      case str ~ None => PlainLiteral(dequote(str))
      case str ~ Some(code) => Text(dequote(str), Language(code))
    }

  def datatypeString: Parser[Term] =
    "\"[^\"]+\"".r ~ "^^" ~ "<[^>]+>".r ^^ {
      case value ~ _ ~ dt => DatatypedLiteral(value, mkuri(dt))
    }

  /* why can't I move these methods to an object? */
  /* TODO: escapes */
  def dequote(str: String) = str.substring(1, str.length() - 1)
  def mkuri(str: String) = URI(dequote(str))

  def toFormula(doc: String): Formula = {
    this.parseAll(ntriplesDoc, doc) match {
      case Success(f, _) => f
      case Failure(msg, _) => throw new SyntaxError(msg)
      case Error(msg, _) => throw new SyntaxError(msg)
    }
  }
}
