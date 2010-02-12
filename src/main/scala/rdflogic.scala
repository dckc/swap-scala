package org.w3.swap.rdflogic

import org.w3.swap
import swap.rdf.RDFNodeBuilder
import swap.logic1.{Term, FunctionTerm, Variable}
import Term.Subst
import swap.logic1ec.{Exists, And, Atomic, ECProver, ECFormula}

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
trait TermNode extends RDFNodeBuilder {
  type Node = Term
  type SubjectNode = Term
  type Label = Name

  def uri(i: String) = Name(i)

  type Literal = Term
  def plain(s: String, lang: Option[Symbol]) = Plain(s, lang)
  def typed(s: String, dt: String): Literal = Data(s, Name(dt))
  def xmllit(e: scala.xml.NodeSeq): Literal = XMLLit(e)
}

object RDFLogic extends ECProver with RDFXMLTerms {
  import swap.rdf.Vocabulary

  def scope(taken: Iterable[Variable]): (Variable => Variable) = {
    val vars = new Scope()
    return {
      case v: XMLVar => vars.fresh(v.sym.name)
      case _ => vars.fresh("x")
    }
  }

  def atom(s: Term, p: Term, o: Term): Atomic = {
    Atomic('holds, List(s, p, o))
  }
  def atom(arc: (Term, Term, Term)): Atomic = {
    Atomic('holds, List(arc._1, arc._2, arc._3))
  }

  def graphFormula(arcs: Iterable[Arc]): ECFormula = {
    val atoms = arcs.toSeq.map { case (s, p, o) => atom(s, p, o) }
    val g = And(atoms)
    val vars = variables(g)
    if (vars.isEmpty) g else Exists(vars, g)
  }
}
