package org.w3.swap.rdflogic

import org.w3.swap
import swap.rdf.RDFGraphParts
import swap.logic0.Formula
import swap.logic1.{Term, FunctionTerm, Variable}
import Term.Subst
import swap.logic1ec.{Exists, And, Atomic, ECLogic}

/**
 * RDF has only ground, 0-ary function terms.
 */
abstract class Ground extends FunctionTerm {
  override def fun = this
  override def args = Nil
  override def variables = Set()
  override def subst(s: Subst) = this
}

case class Name(n: String) extends Ground
case class Plain(s: String, lang: Option[Symbol]) extends Ground
case class Data(lex: String, dt: Name) extends Ground
case class XMLLit(content: scala.xml.NodeSeq) extends Ground


/**
 * Implement RDF Nodes (except BlankNode) using FOL function terms
 */
trait TermNode extends RDFGraphParts {
  type Node = Term
  type SubjectNode = Term
  type Label = Name

  def uri(i: String) = Name(i)

  type Literal = Term
  def plain(s: String, lang: Option[Symbol]) = Plain(s, lang)
  def typed(s: String, dt: String): Literal = Data(s, Name(dt))
  def xmllit(e: scala.xml.NodeSeq): Literal = XMLLit(e)
}

object RDFLogic extends ECLogic with TermNode {
  import swap.rdf.Vocabulary
  val nilterm = Name(Vocabulary.nil)

  def graphFormula(arcs: Iterable[Arc]): Formula = {
    val atoms = arcs.toSeq.map {
      case (s, p, o) => Atomic('holds, List(s, p, o))
    }
    val g = And(atoms)
    Exists(variables(g), g)
  }
}
