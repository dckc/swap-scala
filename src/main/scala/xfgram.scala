package org.w3.swap.grammar

/**
 * <a href="http://www.w3.org/TR/2004/REC-xml11-20040204/#sec-notation"
 * >XML formal grammar notation</a>
 */

/* Parsers brings magic such as ~ and ^^ */
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.annotation.tailrec

case class Rule(id: String, name: Symbol, body: Expr)

class XMLFormalGrammar extends RegexParsers {
  import java.lang.Integer.parseInt

  override val whiteSpace = "[ \t\n\r]*".r

  def grammar: Parser[List[Rule]] = rep(rule)

  def rule: Parser[Rule] = ("\\[\\w+\\]".r ~ id ~
			    ("::=" ~> alternatives) ) ^^ {
    case id ~ name ~ body => Rule(chop(id, 1), name, body)
  }

  /*
   * I don't see a spec for the names in this notation,
   * so let's use a letter followed by anything that's not
   * space or operator/punctuation.
   */
  def id: Parser[Symbol] = "[a-zA-Z][^\\s:?*()]*".r ^^ { case i => Symbol(i) }

  def alternatives: Parser[Alternatives] = repsep(sequence, "|") ^^ {
    case seqs => Alternatives(seqs)
  }

  def sequence: Parser[Sequence] = (
    x_star ~ sequence ^^ {
      case xs ~ seq => Sequence(xs :: seq.items) }
    | item ~ sequence ^^ {
      case i ~ seq => Sequence(i :: seq.items) }
    | success(Concat(Nil))
  )

  def item = lit | range | group | id ^^ { case i => ID(i) }

  def lit: Parser[Item] = (
    "(\"[^\"]+\")|('[^\']+')".r ^^ {
      case l if l.length == 3 => {
	val c = l.charAt(1)
	CharClass(List((c, c)))
      }
      case l => Lit(chop(l, 1))
    }

    | "#x([0-9A-F]+)".r ^^ {
      case xxn => {
	val c = parseInt(xxn.substring(2), 16).toChar
	CharClass(List((c, c)))
      }
    }
  )
  protected def chop(s: String, i: Int) = s.substring(i, s.length - i)

  val range2_4 = "\\[#x([0-9A-F]+)-#x([0-9A-F]+)\\]".r
  val rangeA_B = "\\[(.)-(.)\\]".r

  def range: Parser[Item] = (
    range2_4 ^^ {
      case range2_4(lo, hi) =>
	val lohi = (parseInt(lo, 16).toChar, parseInt(hi, 16).toChar)
	CharClass(List(lohi))
    }
    | rangeA_B ^^ {
      case rangeA_B(lo, hi) => CharClass(List((lo.charAt(0), hi.charAt(0))))
    }
  )

  def group: Parser[Item] = "(" ~> alternatives <~ ")" ^^ {
    case i: Item => i
    case e => Group(e)
  }

  def x_star: Parser[Item] = item <~ "*" ^^ { case i => Rep0n(i) }
}

object EBNF {
  def xml_notation(notation: String): List[Rule] = {
    val p = new XMLFormalGrammar()
    p.parseAll(p.grammar, notation) match {
      case p.Success(l, _) => l
      case oops => List(Rule("0", 'oops, Lit("" + oops)))
    }
  }

  def ruleMap(rules: List[Rule]): Map[Symbol, Expr] = {
    rules match {
      case Nil => Map()
      case rule :: rest => ruleMap(rest) + (rule.name -> rule.body)
    }
  }

  /**
   * Replace non-terminals by their rule bodies.
   * @return: None if there's a loop, i.e. if the Expr is not regular
   */
  def regex(e: Expr, g: Map[Symbol, Expr]): Option[Expr] = {
    def recur(e: Expr, seen: List[Symbol]): Option[Expr] = {
      e match {
	case ID(i) => if (seen contains i) None else recur(Group(g(i)),
							   i :: seen)
	case Choice(ei) =>
	  ei.foldLeft(Some(Choice(Nil)): Option[Alternatives]) {
	    case (None, _) => None
	    case (Some(a), b) => recur(b, seen) match {
	      case None => None
	      case Some(bout: Alternatives) =>
		Some(Alternatives(a.choices ++ bout.choices))
	      case x => throw new Exception("unexpected expr:" + x)
	    }
	  }

	case Concat(ei) =>
	  ei.foldLeft(Some(Concat(Nil)): Option[Sequence]) {
	  case (None, _) => None
	  case (Some(a), b) => recur(b, seen) match {
	    case None => None
	    case Some(bout: Sequence) => Some(Sequence(a.items ++ bout.items))
	    case x => throw new Exception("unexpected expr:" + x)
	  }
	}

	case Rep0n(e) => recur(e, seen) match {
	  case None => None
	  case Some(x: Item) => Some(Rep0n(x))
	  case x => throw new Exception("unexpected expr:" + x)
	}

	case Group(e) => recur(e, seen) match {
	  case None => None
	  case Some(x) => Some(Group(x))
	}

	// scalaq: what's the idiom for combining these?
	case e: CharClass => Some(e)
	case e: Lit => Some(e)
      }
    }

    recur(e, Nil)
  }

  def simplify(e: Expr) = e match {
    case a: Alternatives => choose(a, Alternatives(Nil))
  }

  def choose(a1: Alternatives, a2: Alternatives): Alternatives = {
    a1.choices ++ a2.choices match {
      case Lit(s) :: CharClass(l2) :: rest if s.length == 1 => {
	val c = s.charAt(0)
	choose(CharClass((c, c) :: l2), Alternatives(rest))
      }

      case CharClass(l1) :: CharClass(l2) :: rest =>
	choose(CharClass(l1 ++ l2), Alternatives(rest))

      case choices =>
	Alternatives(choices.map(concat(_, Concat(Nil))))
    }
  }

  def concat(s1: Sequence, s2: Sequence): Sequence = {
    (s1.items ++ s2.items) match {
      case Lit(s1) :: Lit(s2) :: rest =>
	concat(Lit(s1 + s2), Sequence(rest))
      case CharClass(List((lo, hi))) :: rest if lo == hi =>
	concat(Lit(lo.toString), Sequence(rest))
      case items => Sequence(items)
    }
  }

}


object XMLName {
  // http://www.w3.org/TR/2008/REC-xml-20081126/#NT-Name
  val notation = """
[4]   	NameStartChar	   ::=   	":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
[4a]   	NameChar	   ::=   	NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
[5]   	Name	   ::=   	NameStartChar (NameChar)*
"""
  // " help emacs

  val rules = EBNF.xml_notation(notation)
}


sealed abstract class Expr

sealed abstract class Alternatives extends Expr {
  def choices: List[Sequence]
}
case class Choice(ei: List[Sequence]) extends Alternatives {
  require (ei.lengthCompare(1) != 0)
  def choices = ei
}
object Alternatives {
  def apply(branches: List[Sequence]): Alternatives = {
    branches match {
      case seq :: Nil => seq
      case _ => Choice(branches)
    }
  }
}

sealed abstract class Sequence extends Alternatives {
  override def choices = List(this)
  def items: List[Item]
}
case class Concat(ei: List[Item]) extends Sequence{
  require (ei.lengthCompare(1) != 0)
  def items = ei
}

object Sequence {
  def apply(items: List[Item]): Sequence = {
    items match {
      case item :: Nil => item
      case _ => Concat(items)
    }
  }
}
sealed abstract class Item extends Sequence{
  def items = List(this)
}
case class Rep0n(e: Item) extends Item
//case class Rep1n(e: Item) extends Sequence
//case class Rep1(e: Item) extends Sequence
case class ID(i: Symbol) extends Item
case class Lit(s: String) extends Item
case class CharClass(ranges: List[(Char, Char)]) extends Item
//case class Except(all: Item, but: Item) extends Item
case class Group(e: Expr) extends Item
