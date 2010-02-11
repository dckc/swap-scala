package org.w3.swap.rdfxml

import org.w3.swap
import swap.logic1.Variable

/**
 * Logical variables for RDF.
 * @param n: an XML name. TODO: assert this
 */
case class BlankNode(val n: String, val qual: Option[Int]) extends Variable {
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
  val varstack = new mutable.Stack[BlankNode]
  vars foreach {
    case v @ BlankNode(n, x) if safeName(n) == n => varstack.push(v)
    case v @ BlankNode(n, x) => varstack.push(fresh(n))
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
   * Return a BlankNode for this name, creating one if necessary.
   * @return: the same BlankNode given the same name.
   */
  def byName(name: String): BlankNode = {
    var safe = safeName(name)
    varstack.find { v => v.quote().toString() == safe } match {
      case None => fresh(safe)
      case Some(v) => v
    }
  }

  /**
   * @param suggestedName: an XML name
   * @return an XML name unique to this scope
   */
  def fresh(suggestedName: String): BlankNode = {
    assert(suggestedName.length > 0)

    val baseName = safeName(suggestedName)

    val b = {
      val seen = varstack.exists { v => v.quote().toString() == baseName }
      if (seen) BlankNode(baseName, Some(varstack.size))
      else BlankNode(baseName, None)
    }

    varstack.push(b)
    b
  }
}
