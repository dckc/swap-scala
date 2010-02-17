package org.w3.swap.grammar

/**
 * <a href="http://www.w3.org/TR/2004/REC-xml11-20040204/#sec-notation"
 * >XML formal grammar notation</a>
 */

/* Parsers brings magic such as ~ and ^^ */
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.annotation.tailrec

case class Rule(id: String, name: String, body: Expr)

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
  def id: Parser[String] = "[a-zA-Z][^\\s:?*()]*".r

  def alternatives: Parser[Alternatives] = (
    sequence ~ ("|" ~> alternatives) ^^ {
      case CharClass(l1) ~ CharClass(l2) => CharClass(l1 ++ l2)
      case hd ~ tl => Alternatives(hd, tl)
    }
    | sequence ^^ { case s => s }
    | success(Choice(Nil))
  )

  def sequence: Parser[Sequence] = (
    x_star ~ sequence ^^ {
      case xs ~ seq => Sequence.concat(xs, seq) }
    | item ~ sequence ^^ {
      case i ~ s => Sequence.concat(i, s) }
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

  def x_star: Parser[Sequence] = item <~ "*" ^^ { case i => Rep0n(i) }
}


object XMLName {
  // http://www.w3.org/TR/2008/REC-xml-20081126/#NT-Name
  val notation = """
[4]   	NameStartChar	   ::=   	":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
[4a]   	NameChar	   ::=   	NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
[5]   	Name	   ::=   	NameStartChar (NameChar)*
"""
  // " help emacs

  val p = new XMLFormalGrammar()
  val rules = p.parseAll(p.grammar, notation) match {
    case p.Success(l, _) => l
    case oops => List(Rule("0", "oops!", Lit("" + oops)))
  }
}


sealed abstract class Expr

sealed abstract class Alternatives extends Expr {
  def choices: List[Sequence]
}
case class Choice(ei: List[Sequence]) extends Alternatives {
  def choices = ei
}
object Alternatives {
  def apply(hd: Sequence, tl: Alternatives): Alternatives = {
    val hdi = hd match {
      case i: Item => i
      case x => Group(x)
    }

    tl.choices match {
      case Nil => hd
      case more => Choice(hdi :: more)
    }
  }
}

sealed abstract class Sequence extends Alternatives {
  override def choices = List(this)
  def items: List[Item]
}
case class Concat(ei: List[Item]) extends Sequence{
  require(ei.length != 1)
  def items = ei
}

object Sequence {
  def concat(s1: Sequence, s2: Sequence): Sequence = {
    val l = s1.items ++ s2.items
    if (l.length == 1) l(0) else Concat(l)
  }
}
sealed abstract class Item extends Sequence{
  def items = List(this)
}
case class Rep0n(e: Item) extends Item
//case class Rep1n(e: Item) extends Sequence
//case class Rep1(e: Item) extends Sequence
case class ID(i: String) extends Item
case class Lit(s: String) extends Item
case class CharClass(ranges: List[(Char, Char)]) extends Item
//case class Except(all: Item, but: Item) extends Item
case class Group(e: Expr) extends Item
