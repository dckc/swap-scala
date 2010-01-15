package org.w3.swap.ntriples

/* Parsers brings magic such as ~ and ^^ */
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.collection.immutable.ListSet

import org.w3.swap
import swap.logic.{Formula, And, Exists, Term, Variable}
import swap.rdf.{URI, BlankNode, Holds, AbstractSyntax, Vocabulary}
import AbstractSyntax.{plain, data, text}


/**
 * Parser for N-Triples, a simple line-oriented RDF format.
 *
 * The language is specified in
 * <a href="http://www.w3.org/TR/2004/REC-rdf-testcases-20040210/#ntriples"
 * >section 3. N-Triples of <cite>RDF Test Cases</cite></a>.
 */
class NTriplesParser extends NTriplesStrings {
  /**
   * Whitespace is explicit in the N-Triples grammar.
   */
  override def skipWhitespace = false

  def ntripleDoc: Parser[Formula] = rep(line) ^^ {
    case lines => {
      val atoms = for(Some(f) <- lines) yield f
      val vars = atoms.flatMap(f => f.variables)
      if (vars.isEmpty) And(atoms)
      else { Exists(ListSet.empty[Variable] ++ vars, And(atoms)) }
    }
  }

  def line: Parser[Option[Formula]] =
    ws_s ~> opt( comment | triple ) <~ eoln ^^ {
      case Some(maybef) => maybef
      case None => None
    }
      

  def comment: Parser[Option[Formula]] = """#[^\n\r]*""".r ^^ {
    case _ => None }

  def triple: Parser[Option[Formula]] =
    subject ~ ws_p ~ predicate ~ ws_p ~ `object` <~ ws_s <~ "." <~ ws_s ^^ {
    case s~_~p~_~o => Some(Holds(s, p, o))
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

  /**
   * Spec says: ws  	::=  	space | tab
   * This includes repetition
   */
  def ws_p: Parser[String] = """[ \t]+""".r
  def ws_s: Parser[String] = """[ \t]*""".r

  def eoln: Parser[String] = """\r|\n|(?:\r\n)""".r

  /**
   * TODO: move this to test code.
   */
  def toFormula(doc: String): Formula = {
    import swap.logic.Forall

    this.parseAll(ntripleDoc, doc) match {
      case Success(f, _) => f
      case other => {  // not RDF
	println("@@syntax error in toFormula" + other)
	Forall(Set.empty, And(Nil))
      }
    }
  }

}

/* this is factored out for re-use in n3 */
class NTriplesStrings extends RegexParsers {
  def literal: Parser[Term] = string ~ opt("@"~language | "^^"~datatype) ^^ {
    case str ~ None => plain(dequote(str))

    case str ~ Some("@" ~ lang) => text(dequote(str), lang.substring(1))

    case lex ~ Some("^^" ~ dt) => data(lex, mkuri(dt))
  }

  def string: Parser[String] = "\"[^\"\n]+\"".r ^^ { case s => dequote(s) }

  def language: Parser[String] = "[a-z]+(-[a-z0-9]+)*".r

  def datatype: Parser[String] = "^^<[^>]+>".r

  protected def mkuri(str: String) = URI(dequote(str))

  protected def dequote(str: String) = {
    assert(!str.contains("\\"), "TODO: escapes in URIs, strings")
    str.substring(1, str.length() - 1)
  }
}
