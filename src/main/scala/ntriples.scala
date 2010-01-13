package org.w3.swap.ntriples

/* Parsers brings magic such as ~ and ^^ */
import scala.util.parsing.combinator.{Parsers, RegexParsers}

import org.w3.swap.logic.{Formula, And, Exists, Term}
import org.w3.swap.rdf.{URI, BlankNode, AbstractSyntax, Vocabulary}
import AbstractSyntax.{atom, plain, data, text}


/**
 * Parser for N-Triples, a simple line-oriented RDF format.
 *
 * The language is specified in
 * <a href="http://www.w3.org/TR/2004/REC-rdf-testcases-20040210/#ntriples"
 * >section 3. N-Triples of <cite>RDF Test Cases</cite></a>.
 */
class NTriplesParser extends NTriplesStrings {
  def ntripleDoc: Parser[Formula] = phrase(rep(line)) ^^ {
    case List() => And(List())
    case atoms => {
      val vars = atoms.flatMap(f => f.variables)
      if (vars.isEmpty) And(atoms)
      else { Exists(vars.toList.removeDuplicates, And(atoms)) }
    }
  }

  /**
   * Spec says: line  	::=  	ws* ( comment | triple )? eoln
   * but we want to make sure we get a formula, so we skip
   * until we get one
   */
  def line: Parser[Formula] = rep(ws_s | comment_eoln) ~> triple <~ eoln

  /**
   * We'll handle whitespace explicitly.
   */
  override val whiteSpace = "".r

  /**
   * combine comment and eoln
   */
  def comment_eoln: Parser[String] = "#[^\n\r]*(?:\n|\r|(?:\r\n))"

  def triple: Parser[Formula] =
    subject ~ ws_p ~ predicate ~ ws_p ~ `object` <~ ws_s <~ "." <~ ws_s ^^ {
    case s~_~p~_~o => atom(s, p, o)
  }

  def subject: Parser[Term] = uriref | nodeID

  def predicate: Parser[Term] = uriref

  def `object`: Parser[Term] = uriref | nodeID | literal

  /*
   * Spec says:
   * uriref  	::=  	'<' absoluteURI '>'
   * absoluteURI  	::=  	character+ with...
   *
   * TODO: restrict to US-ASCII
   */
  def uriref: Parser[Term] = "<[^>]+>".r ^^ {
    case str => mkuri(str)
  }

  /*
   * Spec says:
   * nodeID  	::=  	'_:' name
   * name  	::=  	[A-Za-z][A-Za-z0-9]*
   */
  def nodeID: Parser[Term] = "_:[A-Za-z][A-Za-z0-9]*".r ^^ {
    case str => BlankNode(str, None)
  }

  def literal: Parser[Term] = langString | datatypeString

  /**
   * Spec says: ws  	::=  	space | tab
   * This includes repetition
   */
  def ws_p: Parser[String] = "[ \t]+"
  def ws_s: Parser[String] = "[ \t]*"

  def eoln: Parser[String] = """\n|\r|(?:\r\n)""".r

  /**
   * TODO: move this to test code.
   */
  def toFormula(doc: String): Formula = {
    this.parseAll(ntripleDoc, doc) match {
      case Success(f, _) => f
      case _ => And(Nil)
    }
  }

}

/* this is factored out for re-use in n3 */
class NTriplesStrings extends RegexParsers {
  /* fold in language */
  def langString: Parser[Term] =
    "\"[^\"\r\n]+\"".r ~ opt("@[a-z]+(-[a-z0-9]+)*".r) ^^ {
      case str ~ None => plain(dequote(str))
      case str ~ Some(code) => text(dequote(str), code.substring(1))
    }

  def datatypeString: Parser[Term] =
    "\"[^\"\n]+\"".r ~ "^^" ~ "<[^>]+>".r ^^ {
      case value~_~dt => data(value, mkuri(dt))
    }

  protected def mkuri(str: String) = new URI(dequote(str))

  protected def dequote(str: String) = {
    assert(!str.contains("\\"), "TODO: escapes in URIs, strings")
    str.substring(1, str.length() - 1)
  }
}
