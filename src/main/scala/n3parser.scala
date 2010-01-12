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
    return base + ref // TODO: real URI combine
  }
}

object N3AbstractSyntax{
  import logicalsyntax.{NotNil, Term, Variable, Apply}

  def atomic(s: Term, p: Term, o: Term) = NotNil(Apply('holds, List(s, p, o)))

  case class EVar(n: Symbol) extends Variable {
    override def name = n
  }
}  

case class QName(pfx: String, ln: String)

class N3Lex extends NTriplesLex {

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
  val prefix_re = """(?:((?:_[A-Za-z0-9_]+)|(?:[A-Za-z][A-Za-z0-9_]*)):)?"""
  val localname_re = """([A-Za-z][A-Za-z0-9_]*)"""

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
  import N3AbstractSyntax.{atomic, EVar}

  import scala.collection.mutable
  val namespaces = mutable.HashMap[String, String]()

  case class Scope(avars: mutable.Set[Variable],
		   evars: mutable.Set[Variable],
		   line: Int,
		   column: Int
		 )
  val scopes = new mutable.Stack[Scope]()
  scopes.push(Scope(mutable.Set.empty, mutable.Set.empty, 1, 1))

  def document: Parser[Formula] = statementsPeriod ^^ {
    case sts => mkFormula(sts)
  }

  def formulacontent: Parser[Formula] = statements ^^ {
    case sts => mkFormula(sts)
  }

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

  def statementsPeriod: Parser[List[Formula]] =
    rep(statement <~ ".") ^^ {
      case lists => lists.flatMap(fmlas => fmlas)
    }

  val statements: Parser[List[Formula]] =
    repsep(statement, ".") <~ opt(".") ^^ {
      case lists => lists.flatMap(fmlas => fmlas)
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
    case symbols => scopes.top.avars += symbols
  }
  *** */

  def declaration: Parser[List[Formula]] = prefixDecl | keywordsDecl

  def prefixDecl: Parser[List[Formula]] = "@prefix" ~> prefix ~ uriref ^^ {
    case prefix ~ uriref => {
      namespaces.put(prefix, uriref)
      List[Formula]()
    }
  }

  def keywordsDecl: Parser[List[Formula]] =
    "@keywords" ~> repsep(qname, ",") ^^ {
      /** TODO: implement keywords decl. kinda yucky */
      case qnames => List[Formula]()
    }


  def simplestatement: Parser[List[Formula]] =
    term ~ propertylist ^^ {
      case t1 ~ propertylist => propertylist.map(poinv =>
	poinv match {
	  // inverted?
	  case (t2: Term, t3: Term, false) => atomic(t1, t2, t3)
	  case (t2: Term, t3: Term, true) => atomic(t3, t2, t1)
	})
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

    | ("is" ~> term <~ "of") ~ repsep(term, ",") ^^ {
      case t2 ~ tn => tn.map(ti => (t2, ti, true))
    }
  )

    // TODO: paths
  def term: Parser[Term] = (
    symbol
    | literal
/* ******
    | evar ^^ { case name =>
      BlankNode(name, Some((scopes.top.line, scopes.top.column))) }
    | uvar ^^ { case name => EVar(Symbol(name)) } // TODO: uvar scope
    | "{" ~> formulacontent <~ "}" ^^ {
      case fmlas => Literal("{...@@...}") // TODO: {} scopes/ terms
    }
    | "[" ~> propertylist <~ "]" ^^ {
      case props => Literal("[...@@...]") // TODO: oops... Parser[(Term, List(Term, Term))]?
    }
    | "(" ~> rep(term) <~ ")" ^^ {
      case items => Apply('list, items) // TODO: first/rest for lists?
    }
*/
  )

  def symbol: Parser[Term] = (
    uriref ^^ { case ref => URI(URISyntax.combine(baseURI, ref)) }
    | qname ^^ { case QName(p, l) => URI(/*@@namespaces(p)*/ "data:" + l) }
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

