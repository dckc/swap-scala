package org.w3.swap.n3

import java.math.BigDecimal
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.collection.immutable.ListSet

import org.w3.swap
import swap.uri.Util.combine
import swap.rdf.{RDFGraphParts}
import swap.turtle.{TurtleLex, CheckedParser}

trait N3LogicTerms extends RDFGraphParts {
  type Term = Node
  type V <: Term
  type BlankNode = V
  type SubjectNode = Term
}

trait N3TermBuilder extends N3LogicTerms {

  lazy val log_implies = uri("http://www.w3.org/2000/10/swap/log#implies")
  lazy val log_or = uri("http://www.w3.org/2000/10/swap/log#or")
  lazy val rdf_type = uri(swap.rdf.Vocabulary.`type`)
  lazy val owl_sameAs = uri("http://www.w3.org/2002/07/owl#sameAs")

  /**
   * @forAll <xyz> makes <xyz> into a variable rather than a logical name.
   */
  def uri(i: String): Term
  def typed(lex: String, dt: String): Term
  def plain(lex: String, lang: Option[Symbol]): Term

  /**
   * Existential variable in the current scope
   */
  def byName(name: String): V
  def fresh(hint: String): V

  /**
   * Univerals variable in the outermost scope
   */
  def universal(name: String): V
  /**
   * Universal quanitifiation in the current scope.
   */
  def declare(name: String)

  def constant(value: Boolean): Literal
  def constant(value: Int): Literal
  def constant(value: Double): Literal
  def constant(value: BigDecimal): Literal

  def pushScope()
  def addStatement(s: Term, p: Term, o: Term)
  def mkList(items: List[Term]): Term
  def popScope(): Term
}


/**
 * N3Syntax is an abstract parser for Notation3 text.
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
 */
abstract class N3Syntax(var baseURI: String) extends N3Lex
with N3TermBuilder with CheckedParser {


  // the default prefix is pre-declared a la @prefix : <#>.
  val namespaces = scala.collection.mutable.HashMap[String, String](
    "" -> combine(baseURI, "#")
  )
  var keywords = Set('a, 'is, 'of, 'has)
  var keywords_declared = false

  def document: Parser[Unit] = rep(statement <~ ".") ^^ { case _ => () }

  def formulacontent: Parser[Unit] = repsep(statement, ".") <~ opt(".") ^^ {
    case sts => ()
  }

  def statement = ( declaration
		   | universal
		   | existential
		   | simplestatement
		 )

  def universal = "@forAll" ~> repsep(symbol, ",") ^^ {
    case symbols => symbols.foreach(declare(_))
  }

  val existential = "@forSome" ~> repsep(symbol, ",") ^^ {
    case symbols => symbols.foreach(byName(_))
  }

  def declaration: Parser[Unit] = (
    k('base) ~> uriref ^^ {
      case ref => baseURI = combine(baseURI, ref)
      ()
    }
    | k('prefix) ~> prefixOptColon ~ uriref ^^ {
      case prefix ~ ref => namespaces.put(prefix, combine(baseURI, ref))
      ()
    }
    | k('keywords) ~> repsep(barename, ",") ^^ {
      // TODO: check that the keywords are known.
      case words => {
	keywords = words.map(Symbol(_)).toSet
	keywords_declared = true
	()
      }
    }
  )

  def mkprops(t1: Term, props: List[(Term, Term, Boolean)]) = {
    for(prop <- props) {
      prop match {
      // inverted?
        case (t2, t3, false) => addStatement(t1, t2, t3)
        case (t2, t3, true) => addStatement(t3, t2, t1)
      }
    }
  }

  def simplestatement: Parser[Unit] = term ~ propertylist ^^ {
    case t1 ~ propertylist => mkprops(t1, propertylist)
  }

  def propertylist: Parser[List[(Term, Term, Boolean)]] =
    repsep(property, ";") ^^ {
      case properties => properties.flatMap(x => x)
    }
    

  def k(sym: Symbol): Parser[Symbol] = (
    ("@" + sym.name) ^^ { case kw => Symbol(kw.substring(1)) }
    | checked(sym.name ^^ { case s => sym }) {
      case (s, in) => (
	if (keywords contains sym) Success(sym, in)
	else Failure("keyword neither decalred nor prefixed by @: " + sym.name,
		     in)
      )
    }
  )

  def property: Parser[List[(Term, Term, Boolean)]] =
    verb ~ repsep(term, ",") ^^ {
      case (v, reversed) ~ o1n => o1n.map(oi => (v, oi, reversed))
    }

  def verb: Parser[(Term,  Boolean)] = (
      "<=" ^^ { case _ => (log_implies, true) }
    | "=>" ^^ { case _ => (log_implies, false) }
    | "=" ^^ { case _ => (owl_sameAs, false) }
    | opt(k('has)) ~> term ^^ { case t => (t, false) }
    | k('a) ^^ { case _ => (rdf_type, false) }
    | k('is) ~> term <~ k('of) ^^ { case t => (t, true) }
  )

    // TODO: paths
  def term: Parser[Term] = (
    symbol ^^ { case s => uri(s) }
    | literal
    | "[" ~> propertylist <~ "]" ^^ {
      case props => {
	val v = fresh("something")
	mkprops(v, props)
	v
      }
    }
    | evar ^^ { case name => byName(combine(baseURI, "#" + name)) }
    | uvar ^^ { case name => universal(combine(baseURI, "#" + name)) }
    | ("{" ^^ { case open => pushScope() }) ~> formulacontent <~ "}" ^^ {
      case fmlas => popScope()
    }
    | "(" ~> rep(term) <~ ")" ^^ {
      case items => mkList(items)
    }
  )

    /* TODO: test whether  'a' allowed in the object spot in n3/turtle.
     * How about as a datatype? */
  def symbol: Parser[String] = (
    uriref ^^ { case ref => combine(baseURI, ref) }

    | checked(qname) { case (q, in) => (
      if (namespaces.isDefinedAt(q._1)) Success(q, in)
      else Error("no such prefix: " + q._1, in)
    ) } ^^ { case (p, l) => namespaces(p) + l }

    | checked(localname_re.r) { case (n, in) => (
      if (keywords_declared) {
	if (!(keywords contains Symbol(n))) Success(n, in)
	else Failure("keyword found where symbol expected: " + n, in)
      }
      else Failure("barename found before @keywords declared: " + n, in)
    ) } ^^ { case n => namespaces("") + n }
  )

  def literal: Parser[Term] = (
    datatypeString ^^ { case (lex, dt) => typed(lex, dt) }
    | quotedStringAtLanguage  ^^ {
      case (s, lang) => plain(s, lang) }
    | numeral
    | k('true) ^^ { case _ => constant(true) }
    | k('false) ^^ { case _ => constant(true) }
  )

  
  def numeral: Parser[Literal] = (
    double ^^ {
      case numeral => constant(numeral.toDouble)
    }
    | decimal ^^ {
      case numeral => constant(Numeral.toDecimal(numeral)) }
    | integer ^^ {
      case numeral => constant(Numeral.toInt(numeral))
    }
  )
}

object Numeral {
  def toInt(numeral: String) = {
    val n = if (numeral.startsWith("+")) numeral.substring(1) else numeral
    java.lang.Integer.parseInt(n)
  }

  def toDecimal(s: String) = new BigDecimal(s)
}

class N3Lex extends swap.turtle.TurtleLex {

  def barename: Parser[String] = localname_re.r

  def uvar: Parser[String] = ("\\?" + localname_re).r ^^ {
    case str => str.substring(1)
  }

  def evar = nodeID
}
