package org.w3.swap.n3

import java.math.BigDecimal
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.collection.immutable.ListSet

import org.w3.swap
import swap.rdf.{URI, BlankNode}
import swap.uri.Util.combine
import swap.logic.{Formula, Atomic,
		   Term, Apply, Variable,
		   AbstractSyntax => Logic }

case class QName(pfx: String, ln: String)

object AbstractSyntax {
  def statement(s: Term, p: Term, o: Term): Formula = {
    NotNil(Apply('holds, List(s, p, o)))
  }
}

case class NotNil(t: Term) extends Atomic {
  override def terms() = List(t)
  def subst(sub: Logic.Subst) = NotNil(t.subst(sub))
  /**
   * sorta makes terms look like formulas.
   * Hark to ACL2 where (and ...) is a term, i.e. a function of booleans
   */
  override def quote() = t.quote()
}

/* TODO: bugfix: re-using NTriplesStrings allows extra spaces,
 * e.g. "abc"@ en and "10" ^^<data:#int>
 * TODO: oops! ntriples doesn't allow qnames in datatype literals
 * */
class N3Lex extends swap.ntriples.NTriplesStrings {
  // treat comments as whitespace
  override val whiteSpace = "(?:[ \t\n\r]|(?:#.*\n?))*".r

  def integer: Parser[Int] = "[+-]?[0-9]+".r ^^ {
    case numeral => {
      val n = if (numeral.startsWith("+")) numeral.substring(1) else numeral
      java.lang.Integer.parseInt(n)
    }
  }

  def double: Parser[Double] = "[+-]?[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)".r ^^ {
    case numeral => java.lang.Double.parseDouble(numeral)
  }

  def decimal: Parser[BigDecimal] = "[+-]?[0-9]+(\\.[0-9]+)".r ^^ {
    case numeral => new BigDecimal(numeral)
  }

  // NTriples doesn't allow relative URI refs; N3 does.
  def uriref: Parser[String] =
    """<([^<>'{}|^`&&[^\x01-\x20]])*>""".r ^^ {
      case str => str.substring(1, str.length()-1)
    }

  // TODO: non-ASCII name characters

  /* note _:xyz is an evar but _a:xyz is a qname */
  val prefix_re = """(?:((?:_[A-Za-z0-9_]+)|(?:[A-Za-z][A-Za-z0-9_]*)|):)"""
  val localname_re = """([A-Za-z][A-Za-z0-9_-]*)"""

  /* TODO: add ? after prefix_re when we do keywords */
  val Qname_re = (prefix_re + localname_re).r
  def qname: Parser[QName] = Qname_re ^^ {
    case str => str match { case Qname_re(p, l) => QName(p, l) }
  }

  def prefix: Parser[String] = prefix_re.r ^^ {
    /* strip off the colon*/
    case str => str.substring(0, str.length() - 1)
  }

  def uvar: Parser[String] = ("\\?" + localname_re).r ^^ {
    case str => str.substring(1)
  }

  def evar: Parser[String] = ("_:" + localname_re).r ^^ {
    case str => str.substring(2)
  }

  // emacs gets confused by this
  def stringLit3: Parser[String] =
    ("\"\"\"" + """(?:[^"\\]+|"|(?:"")|(?:\\[tbnrf\\"]))*"""
              + "\"\"\"" //"emacs
    ).r ^^  {
      // TODO: escapes in triple-quoted strings
      case str => {
	str.substring(3, str.length() - 3)
      }
  }
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
 */
class N3Parser(override val baseURI: String) extends TextRDF(baseURI) {
  def mkstatement(s: Term, p: Term, o: Term): Formula = {
    swap.n3.AbstractSyntax.statement(s, p, o)
  }
}

/**
 * TextRDF abstracts shared part of turtle, n3, and SPARQL syntax.
 * TODO: factor out a turtle parser
 */ 
abstract class TextRDF(val baseURI: String) extends N3Lex {
  import swap.logic.{Formula, Exists, Forall, And,
		     Term, Variable, Apply, Literal}
  import swap.rdf.{BlankNode}
  import swap.rdf.AbstractSyntax.{text, data}

  def mkstatement(s: Term, p: Term, o: Term): Formula

  import scala.collection.mutable
  val namespaces = mutable.HashMap[String, String]()
  val brackets = BlankNode("something", None)

  case class Scope(avars: mutable.Stack[Variable],
		   evars: mutable.Stack[Variable],
		   statements: mutable.Stack[Formula])
  val scopes = new mutable.Stack[Scope]()
  scopes.push(Scope(new mutable.Stack(),
		    new mutable.Stack(),
		    new mutable.Stack()))

  def document: Parser[Formula] = rep(statement <~ ".") ^^ {
    case sts => mkFormula(scopes.top.statements.toList.reverse)
  }

  /* ***untested
  def formulacontent: Parser[Unit] = repsep(statement, ".") <~ opt(".") ^^ {
    case sts => {
      val out = mkFormula(List[Formula]() + scopes.top.statements)
      scopes.pop()
      out
    }
  }
  */

  def mkFormula(statements: List[Formula]): Formula = {
    val f1 = And(statements)
    val s = scopes.top
    val e = new ListSet[Variable]
    (s.avars.isEmpty, s.evars.isEmpty) match {
      case (true, true) => f1
      case (true, false) => Exists(e ++ s.evars, f1)
      case (false, true) => Forall(e ++ s.avars, f1)
      case (false, false) => Forall(e ++ s.avars, Exists(e ++ s.evars, f1))
    }
  }

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

  def prefixDecl: Parser[Unit] = "@prefix" ~> prefix ~ uriref ^^ {
    case prefix ~ ref => {
      namespaces.put(prefix, combine(baseURI, ref))
    }
  }

  def keywordsDecl: Parser[Unit] = "@keywords" ~> repsep(qname, ",") ^^ {
    /** TODO: implement keywords decl. kinda yucky */
    case qnames => None
  }


  def mkprops(t1: Term, props: List[(Term, Term, Boolean)]) = {
    for(prop <- props) {
      val f = prop match {
	// inverted?
	case (t2: Term, t3: Term, false) => mkstatement(t1, t2, t3)
	case (t2: Term, t3: Term, true) => mkstatement(t3, t2, t1)
      }
      scopes.top.statements.push(f)
    }
  }

  def simplestatement: Parser[Unit] = term ~ propertylist ^^ {
    case t1 ~ propertylist => mkprops(t1, propertylist)
  }

  def propertylist: Parser[List[(Term, Term, Boolean)]] =
    repsep(property, ";") ^^ {
      case properties => properties.flatMap(x => x)
    }
    

  def property: Parser[List[(Term, Term, Boolean)]] = (
    // TODO: with keywords, this becomes @has
    opt("has") ~> term ~ repsep(term, ",") ^^ {
      case t2 ~ tn => tn.map(ti => (t2, ti, false))
    }

    | "is" ~> term ~ "of" ~ repsep(term, ",") ^^ {
      case t2 ~ x ~ tn => tn.map(ti => (t2, ti, true))
    }
  )

    // TODO: paths
  def term: Parser[Term] = (
    symbol
    | literal
    | "[" ~> propertylist <~ "]" ^^ {
      case props => {
	val fresh = brackets.fresh()
	scopes.top.evars.push(fresh)
	mkprops(fresh, props)
	fresh
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

  def symbol: Parser[Term] = (
    uriref ^^ { case ref => URI(combine(baseURI, ref)) }
    | qname ^^ { case QName(p, l) => URI(namespaces(p) + l) }
    | "a" ^^ {
      case s => URI("http://www.w3.org/1999/02/22-rdf-syntax-ns#type") }
    | "=" ^^ {
      case s => URI("http://www.w3.org/2002/07/owl#sameAs") }
    /* 
     | "=>" ^^ {
     case s => URI("http://www.w3.org/2000/10/swap/log#implies") }
     */
  )

    // N3, turtle, SPARQL have numeric, boolean literals too
  override def literal: Parser[Term] = (
    // TODO: can langString and datatypeString use """s too?
    // TODO: left factor string-handling.
    string ~ "^^" ~ datatype ^^ { case lex~_~dt => data(lex, mkuri(dt)) }
    | string ~ "@" ~ language ^^ { case s~_~lang => text(s, lang) }
    | string ^^ { case s => Literal(s) }
    | stringLit3 ^^ { case s => Literal(s) }
    | numeral
    | boolean
    )

  def numeral: Parser[Term] = (
    double ^^ { case x => Literal(x) }
    | decimal ^^ { case x => Literal(x) }
    | integer ^^ { case i => Literal(i) }
    )

  def boolean: Parser[Term] = (
    // TODO: with keywords, this is "@true" and "@false"
    "true" ^^ { case b => Literal(true) }
    | "fase" ^^ { case b => Literal(false) }
  )
}

