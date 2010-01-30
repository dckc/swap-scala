package org.w3.swap.rdf

import org.w3.swap
import swap.logic.{Formula, And, Exists}

import scala.xml.{Elem}

class RDFaParser(base: String) {
  import AbstractSyntax.plain

  import scala.collection.mutable
  val statements = new mutable.Stack[Formula]()
  val blank = BlankNode("node", None) // source of fresh variables

  def parse(e: Elem): Formula = {
    walk(e)

    val f1 = And(statements.toList.reverse)
    val vars = f1.variables()
    if (vars.isEmpty) f1
    else Exists(vars, f1)
  }

  def walk(e: Elem) {
    val about = e \ "@about"
    val property = e \ "@property"

    (about.isEmpty, property.isEmpty) match {
      case (false, false) => {
	val subj = URI(swap.uri.Util.combine(base, about.text))
	val pred = URI(CURIE.expand(property.text, e))
	val obj = plain(e.text)
	statements.push(Holds(subj, pred, obj))
      }

      case _ => /* umm... Unit? */
    }

    e.child.foreach {
      case c: Elem => walk(c)
      case _ => /* never mind stuff other than elements */
    }
  }

}

object CURIE {
  import scala.util.matching.Regex

  final val parts = new Regex("([^:]+):(.*)", "prefix", "localname")

  def expand(curie: String, e: Elem): String = {
    curie match {
      case parts(p, l) => {
	val ns = e.getNamespace(p)
	if (ns == null) {
	  throw new NotDefinedError("no such prefix " + p + " on element " + e)
	}
	ns + l
      }
      case _ => {
	assert(false, "not a curie: " + curie)
	""
      }
    }
  }
}
