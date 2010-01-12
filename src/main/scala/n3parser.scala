package org.w3.swap

/*
 * based on
 * http://www.w.w3org/2000/10/swap/grammar/notation3.bnf
 * 1.14 2006/06/22 22:03:21
 */

import java.math.BigDecimal
import scala.util.parsing.combinator.{Parsers, RegexParsers}

import rdf2004.URI // TODO: find a better place for URI than rdf2004

object URISyntax {
  def combine(base: String, ref: String): String = {
    if (ref.contains(":")) ref
    else base + ref // TODO: real URI combine
  }
}

object N3AbstractSyntax{
  import logicalsyntax.{NotNil, Term, Variable, Apply}

  def atom(s: Term, p: Term, o: Term) = NotNil(Apply('holds, List(s, p, o)))

  case class EVar(n: Symbol) extends Variable {
    override def name = n
  }
}  

case class QName(pfx: String, ln: String)

class N3Lex extends NTriplesLex {
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
  val localname_re = """([A-Za-z][A-Za-z0-9_]*)"""

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
}

class N3Parser(val baseURI: String) extends N3Lex {
  import logicalsyntax.{Formula, Exists, Forall, And,
			Term, Variable, Apply, Literal}
  import org.w3.swap.rdf2004.BlankNode
  import N3AbstractSyntax.{atom, EVar}

  import scala.collection.mutable
  val namespaces = mutable.HashMap[String, String]()

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
    (s.avars.isEmpty, s.evars.isEmpty) match {
      case (true, true) => f1
      case (true, false) => Exists(s.evars.toList, f1)
      case (false, true) => Forall(s.avars.toList, f1)
      case (false, false) => Forall(s.avars.toList, Exists(s.evars.toList, f1))
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
      namespaces.put(prefix, URISyntax.combine(baseURI, ref))
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
	case (t2: Term, t3: Term, false) => atom(t1, t2, t3)
	case (t2: Term, t3: Term, true) => atom(t3, t2, t1)
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
    

  val property: Parser[List[(Term, Term, Boolean)]] = (
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
	// TODO: prove that evars.size is sufficiently unique
	val v = BlankNode("e", Some(scopes.top.evars.size))
	scopes.top.evars.push(v)
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

  def symbol: Parser[Term] = (
    uriref ^^ { case ref => URI(URISyntax.combine(baseURI, ref)) }
    | qname ^^ { case QName(p, l) => URI(namespaces(p) + l) }
    | "a" ^^ { case s => URI(rdf2004.rdf + "type")
  )

  // N3, turtle, SPARQL have numeric, boolean literals too
  override val literal: Parser[Term] = (
    langString | datatypeString
    | numeral
    | boolean
    )

  val numeral: Parser[Term] = (
    double ^^ { case x => Literal(x) }
    | decimal ^^ { case x => Literal(x) }
    | integer ^^ { case i => Literal(i) }
    )

  val boolean: Parser[Term] = (
    // TODO: with keywords, this is "@true" and "@false"
    "true" ^^ { case b => Literal(true) }
    | "fase" ^^ { case b => Literal(false) }
  )
}

