package org.w3.swap.n3

import java.math.BigDecimal
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.collection.immutable.ListSet

import org.w3.swap
import swap.uri.Util.combine
import swap.logic1ec.And
import swap.rdf.RDFNodeBuilder
import swap.turtle.TurtleLex

class N3Lex extends swap.turtle.TurtleLex {

  def uvar: Parser[String] = ("\\?" + localname_re).r ^^ {
    case str => str.substring(1)
  }

  def evar = nodeID
}

/**
 * N3Parser builds a Formula from Notation3 text.
 *
 * This is a fairly straightforward transcription of
 * <a href="http://www.w3.org/2000/10/swap/grammar/notation3.bnf"
 * >notation3.bnf</a> (v1.14 2006/06/22 22:03:21) using
 * <a href="http://www.artima.com/forums/flat.jsp?forum=276&thread=229061"
 * >Scala Parser Combinators</a>. (see Chapter 31 Combinator Parsing
 * in <cite>Programming in Scala</cite> by Odersky, Spoon, and Venners.)
 *
 * See also:
 *
 * <ul>
 * <li>
 * <cite><a href="http://www.w3.org/2000/10/swap/Primer"
 * >Primer: Getting into RDF & Semantic Web using N3</a></cite>
 * and
 * </li>
 * <li>
 * <cite><a href="http://www.w3.org/DesignIssues/Notation3"
 * >Notation3 (N3): A readable RDF syntax</a></cite>
 * </li>
 * <li><a href="http://www.w3.org/2000/10/swap/test/n3parser.tests"
 * >n3parser.tests</a> in <a href="http://www.w3.org/2000/10/swap/"
 * >python swap</a>
 * </ul>
 * @param baseURI absolute URI to use to resolve relative URI references
 *                for example:
 *                val p = N3Parser("data:")
 *                p.parseAll(p.document, "<#x> <#prop> 23")
 *                (holds (data:#x) (data:#prop) 23)
 *
 *                see also combine
 * 
 * @author <a href="http://www.w3.org/People/Connolly/">Dan Connolly</a>

class N3Parser(override val baseURI: String) extends N3Syntax(baseURI) {
  import swap.rdflogic.RDFLogic

  def mkatom(s: Node, p: Node@@@, o: Term): Formula = {
    RDFLogic.atom(s, p, o)
  }
}
  */

abstract class N3Syntax(val baseURI: String) extends N3Lex with RDFNodeBuilder {

  def fresh(hint: String): BlankNode
  def byName(name: String): BlankNode
  def constant(value: Boolean): Literal
  def constant(value: Int): Literal
  def constant(value: Double): Literal
  def constant(value: BigDecimal): Literal

  def pushScope(): Unit
  def popScope(): Unit
  def addAtom(s: Node, p: Node, o: Node): Unit


  val namespaces = scala.collection.mutable.HashMap[String, String]()

  def document: Parser[Unit] = rep(statement <~ ".") ^^ { case _ => () }

  /* ***untested
  def formulacontent: Parser[Unit] = repsep(statement, ".") <~ opt(".") ^^ {
    case sts => {
      val out = mkFormula(List[Formula]() + scopes.top.statements)
      scopes.pop()
      out
    }
  }
  */

  def statement = ( declaration
		   /**** test later
		   * universal
		   * existential
		   */
		   | simplestatement )

    /**** test later
  val universal = "@forAll" >~ repsep(symbol, ",") <~ "." ^^ {
    case symbols => scopes.top.avars += symbols
  }

  val universal = "@forAll" >~ repsep(symbol, ",") <~ "." ^^ {
    case symbols => scopes.top.evars += symbols
  }
  *** */

  def declaration: Parser[Unit] = prefixDecl | keywordsDecl

  def prefixDecl: Parser[Unit] = "@prefix" ~> prefixOptColon ~ uriref ^^ {
    case prefix ~ ref => {
      namespaces.put(prefix, combine(baseURI, ref))
    }
  }

  def keywordsDecl: Parser[Unit] = "@keywords" ~> repsep(qname, ",") ^^ {
    /** TODO: implement keywords decl. kinda yucky */
    case qnames => None
  }

  def mkprops(t1: Node, props: List[(Node, Node, Boolean)]) = {
    for(prop <- props) {
      prop match {
      // inverted?
        case (t2, t3, false) => addAtom(t1, t2, t3)
        case (t2, t3, true) => addAtom(t3, t2, t1)
      }
    }
  }

  def simplestatement: Parser[Unit] = term ~ propertylist ^^ {
    case t1 ~ propertylist => mkprops(t1, propertylist)
  }

  def propertylist: Parser[List[(Node, Node, Boolean)]] =
    repsep(property, ";") ^^ {
      case properties => properties.flatMap(x => x)
    }
    

  def property: Parser[List[(Node, Node, Boolean)]] = (
    // TODO: with keywords, this becomes @has
    opt("has") ~> term ~ repsep(term, ",") ^^ {
      case t2 ~ tn => tn.map(ti => (t2, ti, false))
    }

    | "is" ~> term ~ "of" ~ repsep(term, ",") ^^ {
      case t2 ~ x ~ tn => tn.map(ti => (t2, ti, true))
    }
  )

    // TODO: paths
  def term: Parser[Node] = (
    symbol
    | literal
    | "[" ~> propertylist <~ "]" ^^ {
      case props => {
	val v = fresh("something")
	mkprops(v, props)
	v
      }
    }
/* ******
    | evar ^^ { case name =>
      BlankNode(name, Some((scopes.top.line, scopes.top.column))) }
    | uvar ^^ { case name => EVar(Symbol(name)) } // TODO: uvar scope
    | "{" ~> formulacontent <~ "}" ^^ {
      case fmlas => Literal("{...@@...}") // TODO: {} scopes/ terms
    }
    | "(" ~> rep(term) <~ ")" ^^ {
      case items => Apply('list, items) // TODO: first/rest for lists?
    }
*/
  )

    /* checked wraps a Parser[T] with a check on its results
     * */
  protected def checked[T](p: => Parser[T])(
    check: (T, Input) => ParseResult[T]): Parser[T] = Parser {
      in => p(in) match {
	case s @ Success(x, in) => check(x, in)
	case ns => ns
      }
    }

    /* TODO: test whether  'a' allowed in the object spot in n3/turtle.
     * How about as a datatype? */
  def symbol: Parser[Label] = (
    uriref ^^ { case ref => uri(combine(baseURI, ref)) }

    | checked(qname) { case (q, in) => (
      if (namespaces.isDefinedAt(q._1)) Success(q, in)
      else Error("no such prefix: " + q._1, in)
    ) } ^^ { case (p, l) => uri(namespaces(p) + l) }

    | "a" ^^ { case s => rdf_type }
    | "=" ^^ { case s => uri("http://www.w3.org/2002/07/owl#sameAs") }
    /* 
     | "=>" ^^ {
     case s => URI("http://www.w3.org/2000/10/swap/log#implies") }
     */
  )

  def literal: Parser[Node] = (
    // TODO: can langString and datatypeString use """s too?
    // TODO: left factor string-handling.
    datatypeString ^^ { case (lex, dt) => typed(lex, dt) }
    | quotedStringAtLanguage  ^^ {
      case (s, lang) => plain(s, lang) }
    | numeral
    | boolean ^^ {
      case "true" => constant(true)
      case "fase" => constant(false)
    }
  )

  
  def numeral: Parser[Literal] = (
    double ^^ {
      case numeral => constant(java.lang.Double.parseDouble(numeral))
    }
    | decimal ^^ {
      case numeral => constant(new BigDecimal(numeral)) }
    | integer ^^ {
      case numeral => {
	val n = if (numeral.startsWith("+")) numeral.substring(1) else numeral
	constant(java.lang.Integer.parseInt(n))
      }
    }
  )
}

