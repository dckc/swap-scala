package org.w3.swap.ntriples

/* Parsers brings magic such as ~ and ^^ */
import scala.util.parsing.combinator.{Parsers, RegexParsers}

import org.w3.swap
import swap.rdfgraph.RDFGraph

/**
 * Parser for N-Triples, a simple line-oriented RDF format.
 *
 * The language is specified in
 * <a href="http://www.w3.org/TR/2004/REC-rdf-testcases-20040210/#ntriples"
 * >section 3. N-Triples of <cite>RDF Test Cases</cite></a>.
 */
abstract class NTriplesSyntax extends NTriplesTerms {
  /**
   * Whitespace is explicit in the N-Triples grammar.
   */
  override def skipWhitespace = false

  def lazyrep[T] (p: Parser[Stream[T]]): Parser[Stream[T]] = (
    p ~ lazyrep(p) ^^ { case hd~tl => hd ++ tl }
    | success(Stream.empty)
  )

  def ntripleDoc: Parser[Stream[Arc]] = lazyrep(line)

  def line: Parser[Stream[Arc]] =
    ws_s ~> opt( comment | triple ) <~ eoln ^^ {
      case Some(Some(f)) => Stream(f)
      case Some(None) => Stream.empty
      case None => Stream.empty
    }
      

  def comment: Parser[Option[Arc]] = """#[^\n\r]*""".r ^^ {
    case _ => None }

  def triple: Parser[Option[Arc]] =
    subject ~ ws_p ~ predicate ~ ws_p ~ `object` <~ ws_s <~ "." <~ ws_s ^^ {
    case s~_~p~_~o => Some((s, p, o))
  }

  def subject: Parser[SubjectNode] = uriref | nodeID

  def predicate: Parser[Label] = uriref

  def `object`: Parser[Node] = uriref | nodeID | literal

  /**
   * Spec says: ws  	::=  	space | tab
   * This includes repetition
   */
  def ws_p: Parser[String] = """[ \t]+""".r
  def ws_s: Parser[String] = """[ \t]*""".r

  def eoln: Parser[String] = """\r|\n|(?:\r\n)""".r
}

abstract class NTriplesTerms extends RegexParsers with RDFGraph {

  def blankNode(id: String): BlankNode

  /*
   * Spec says:
   * uriref  	::=  	'<' absoluteURI '>'
   * absoluteURI  	::=  	character+ with...
   *
   * TODO: restrict to US-ASCII
   */
  def uriref: Parser[Label] = "<[^>]+>".r ^^ {
    case str => uri(dequote(str))
  }

  /*
   * Spec says:
   * nodeID  	::=  	'_:' name
   * name  	::=  	[A-Za-z][A-Za-z0-9]*
   */
  def nodeID: Parser[BlankNode] = "_:[A-Za-z][A-Za-z0-9]*".r ^^ {
    case xxname => blankNode(xxname.substring(2))
  }

  def literal: Parser[Literal] = string ~ opt(language | datatype) ^^ {
    case lex ~ Some(dt: String) => typed(lex, dt)

    case str ~ Some(lang: LanguageTag) => plain(str, Some(lang))

    case str ~ _ => plain(str, None)
  }

  def string: Parser[String] = "\"[^\"\n]+\"".r ^^ { case s => dequote(s) }

  def language: Parser[Symbol] = "@[a-z]+(-[a-z0-9]+)*".r ^^ {
    case atlang => Symbol(atlang.substring(1))
  }

  def datatype: Parser[String] = "^^<[^>]+>".r ^^ {
    // ummm... same dequote for URIs and strings?
    case xxdt => dequote(xxdt.substring(2))
  }

  protected def dequote(str: String) = {
    import swap.StringUtil.{dequote => unescape}
    unescape(str.substring(1, str.length() - 1))
  }
}
