package org.w3.swap.rdflogic

import org.w3.swap
import swap.logic1.Variable

/**
 * Logical variables for RDF.
 * @param n: an XML name. TODO: assert this
 */
case class XMLVar(val n: String, val qual: Option[Int]) extends Variable {
  // TODO: mix in Quotable
  def quote() = sym

  lazy val sym = qual match {
    case None => Symbol(n)
    case Some(x) => Symbol(n + "." + x)
  }
}

class Scope(val vars: Iterable[Variable]) {
  def this() = this(List())

  import scala.collection.mutable
  val varstack = new mutable.Stack[XMLVar]
  vars foreach {
    case v @ XMLVar(n, x) if safeName(n) == n => varstack.push(v)
    case v @ XMLVar(n, x) => varstack.push(fresh(n))
    case _ => varstack.push(fresh("v"))
  }

    /* baseName is a name that does *not* follow the xyx.123 pattern */
  protected def safeName(name: String) = {
    val lastChar = name.substring(name.length-1)
    if("0123456789".contains(lastChar) &&
       name.contains('.')) name + "_"
    else name
  }

  /**
   * Return a XMLVar for this name, creating one if necessary.
   * @return: the same XMLVar given the same name.
   */
  def byName(name: String): XMLVar = {
    var safe = safeName(name)
    varstack.find { v => v.sym.name == safe } match {
      case None => fresh(safe)
      case Some(v) => v
    }
  }

  /**
   * @param suggestedName: an XML name
   * @return an XML name unique to this scope
   */
  def fresh(suggestedName: String): XMLVar = {
    assert(suggestedName.length > 0)

    val baseName = safeName(suggestedName)

    val b = {
      val seen = varstack.exists { v => v.sym.name == baseName }
      if (seen) XMLVar(baseName, Some(varstack.size))
      else XMLVar(baseName, None)
    }

    varstack.push(b)
    b
  }
}

/**
 * Add concrete implementation of BlankNode
 * as well as other RDFGraphParts from TermNode.
 */
trait RDFXMLTerms extends TermNode {
  type BlankNode = XMLVar

  lazy val vars = new Scope()
  def fresh(hint: String) = vars.fresh(hint)
  def byName(name: String) = vars.byName(name)
}
