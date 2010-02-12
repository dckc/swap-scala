package org.w3.swap.turtle

/* Parsers brings magic such as ~ and ^^ */
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.annotation.tailrec

import org.w3.swap
import swap.uri.Util.combine
import swap.rdf.RDFNodeBuilder
import swap.rdf.Vocabulary

/**
 * TurtleParser
 * http://www.w3.org/TeamSubmission/turtle/#sec-grammar-grammar
 */ 
abstract class TurtleSyntax(val initialBase: String)
extends TurtleLex with RDFNodeBuilder {
  def fresh(hint: String): BlankNode
  def byName(name: String): BlankNode

  import scala.collection.mutable
  val namespaces = mutable.HashMap[String, String]()
  var baseaddr = initialBase

  def lazyrep[T] (p: Parser[Stream[T]]): Parser[Stream[T]] = (
    p ~ lazyrep(p) ^^ { case hd~tl => hd ++ tl }
    | success(Stream.empty)
  )

  //TODO: try phrase() again? didn't seem to work, earlier
  def turtleDoc: Parser[Stream[Arc]] = lazyrep(statement)


  def statement: Parser[Stream[Arc]] = (
    directive <~ "." ^^ { case _ => Stream.empty }
    | triples <~ "."
    // | ws +
  )

  def directive: Parser[Unit] = prefixID | base

  def prefixID: Parser[Unit] = "@prefix" ~> prefixOptColon ~ uriref ^^ {
    case prefix ~ ref => {
      namespaces.put(prefix, combine(baseaddr, ref))
    }
  }

  def base: Parser[Unit] = "@base" ~> uriref ^^ {
    case ref => baseaddr = combine(baseaddr, ref)
  }

  def triples: Parser[Stream[Arc]] = subject ~ predicateObjectList ^^ {
    case ((arcs1, s)) ~ ((arcs2, pos)) => {
      arcs1 ++ arcs2 ++ pos.map { case (p, o) => (s, p, o) }
    }
  }

  def predicateObjectList: Parser[(Stream[Arc], List[(Label, Node)])] = (
    repsep(verb ~ objectList, ";") ^^ {
      case vol => {
	val arcs = vol.toStream.flatMap{ case v ~ ((oarcs, ol)) => oarcs }
	val lln = vol.flatMap{ case v ~ ((oarcs, ol)) => ol.map((v, _)) }
	(arcs, lln)
      }
    }
  )

  def objectList: Parser[(Stream[Arc], List[Node])] = (
    repsep(`object`, ",") ^^ {
      case ol => {
	var arcs = ol.toStream.flatMap{ case (arcs, node) => arcs }
	var nodelist = ol.map{ case (arcs, node) => node }
	(arcs, nodelist)
      }
    }
  )
				  
  def verb: Parser[Label] = (
    predicate
    | "a" ^^ { case a => rdf_type }
  )

  def subject: Parser[(Stream[Arc], SubjectNode)] = (
    // ugh! "resource" is a use/mention bug!
    resource ^^ { case s => (Stream.empty, s) }
    | blank
  )

  def predicate = resource

  def `object`: Parser[(Stream[Arc], Node)] = (
    resource ^^ { case o => (Stream.empty, o) }
    | blank
    | literal ^^ { case o => (Stream.empty, o) }
  )

  def literal: Parser[Literal] = (
    datatypeString ^^ { case (lex, dt) => typed(lex, dt) }
    | quotedStringAtLanguage ^^ { case (s, langopt) => plain(s, langopt) }
    | integer ^^ { case num => typed(num, Vocabulary.integer) }
    | double ^^ { case num => typed(num, Vocabulary.double) }
    | decimal ^^ { case num => typed(num, Vocabulary.decimal) }
    | boolean ^^ { case b => typed(b, Vocabulary.boolean) }
    )

  def blank: Parser[(Stream[Arc], SubjectNode)] = (
    nodeID ^^ { case b => (Stream.empty, byName(b)) }
    | "[" ~ "]" ^^ { case bra ~ ket => (Stream.empty, fresh("bk")) }
    | "[" ~> predicateObjectList <~ "]" ^^ {
      case (arcs, pos) => {
	val b = fresh("brackets")
	(arcs ++ pos.map { case (p, o) => (b, p, o) }, b)
      }
    }
    | collection
  )

  def itemListOpt: Parser[(Stream[Arc], List[Node])] = rep(`object`) ^^ {
    case ol => {
      val arcs = ol.toStream.flatMap { case (arcs, node) => arcs }
      val nodes = ol.map { case (arcs, node) => node }
      (arcs, nodes)
    }
  }


  def itemArcs(items: List[Node]): (Stream[Arc], SubjectNode) = {
    items match {
      case Nil => (Stream.empty, rdf_nil)
      case first :: rest => {
	val (arcs1, tail) = itemArcs(rest)
	val cell = fresh("list")
	val arcs = Stream.cons((cell, rdf_first, first),
			       Stream.cons((cell, rdf_rest, tail), arcs1))
	(arcs, cell)
      }
    }
  }

  def collection: Parser[(Stream[Arc], SubjectNode)] = (
    "(" ~> itemListOpt <~ ")" ^^ {
      case (arcs, items) => {
	val (listarcs, listnode) = itemArcs(items)
	(arcs ++ listarcs, listnode)
      }
    }
  )

  def resource: Parser[Label] = (
    uriref ^^ { case ref => uri(combine(baseaddr, ref)) }

    | checked(qname) { case (qn, in) => (
      if (namespaces.isDefinedAt(qn._1)) Success(qn, in)
      else Error("no such prefix: " + qn._1, in)
    ) } ^^ { case (p, l) => uri(namespaces(p) + l) }

  )

  /**
   * checked wraps a Parser[T] with a check on its results
   */
  protected def checked[T](p: => Parser[T])(
    check: (T, Input) => ParseResult[T]): Parser[T] = Parser {
      in => p(in) match {
	case s @ Success(x, in) => check(x, in)
	case ns => ns
      }
    }

}


class TurtleLex extends RegexParsers {
  // treat comments as whitespace
  // this corresponds to ws+ in the turtle spec
  override val whiteSpace = "(?:[ \t\n\r]|(?:#[^\r\n]*))*".r

  def nodeID: Parser[String] = ("_:" + localname_re).r ^^ {
    case str => str.substring(2)
  }

  /* note _:xyz is an evar but _a:xyz is a qname */
  val prefix_re = """(?:((?:_[A-Za-z0-9_]+)|(?:[A-Za-z][A-Za-z0-9_]*)|):)"""
  val localname_re = """([A-Za-z][A-Za-z0-9_-]*)"""

  def prefixOptColon: Parser[String] = prefix_re.r ^^ {
    /* strip off the colon*/
    case str => str.substring(0, str.length() - 1)
  }

  val Qname_re = (prefix_re + localname_re).r
  def qname: Parser[(String, String)] = Qname_re ^^ {
    case Qname_re(p, l) => (p, l)
  }

  /**
   * TODO: uriref escaping
   */
  def uriref: Parser[String] = (
    """<([^<>'{}|^`&&[^\x01-\x20]])*>""".r ^^ {
      case str => str.substring(1, str.length()-1)
    }
  )

  val lang_re = "[a-z]+(?:-[a-z0-9]+)*"
  val string_re = "\"[^\"]*\"" // TODO: fix
  val longString_re = (
    "\"\"\""
    + "(?:[^\"\\\\]+|\"|(?:\"\")|(?:\\\\[tbnrf\\\\\"]))*"
    + "\"\"\""
  )
  val quotedString_re = "(" + string_re + ")|(" + longString_re + ")"
  val qsal_pat = (quotedString_re + "(@" + lang_re + ")?").r
  def stripn(s: String, n: Int) = s.substring(n, s.length - n)

  def quotedStringAtLanguage: Parser[(String, Option[Symbol])] = (
    qsal_pat ^^ {
      case qsal_pat(sq, lsq, lang) => {
	val s = (
	  if (lsq == null) stripn(sq, 1)
	  else stripn(lsq, 3)
	)
	val langopt = if (lang == null) None else Some(Symbol(lang))
	(s, langopt)
      }
    }
  )

  val datatype_pat = ("(" + quotedString_re + ")^^<([^>]*)>").r
  def datatypeString: Parser[(String, String)] = datatype_pat ^^ {
    case datatype_pat(lex, dt) => {
      // TODO: unescaping
      (lex, dt)
    }
  }

  def integer: Parser[String] = "[+-]?[0-9]+".r

  def double: Parser[String] = "[+-]?[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)".r

  def decimal: Parser[String] = "[+-]?[0-9]+(\\.[0-9]+)".r

  def boolean: Parser[String] = "true" | "fase"

}
