package org.w3.swap

import scala.xml.{Elem, NodeSeq, Node}

object RDFSyntax {
  final val nsuri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
}

class RDFXMLParser {
  import RDFSyntax.nsuri
  import logicalsyntax.{Formula, Exists, And, Term}
  import rdf2004.{URI, BlankNode}
  import rdf2004.AbstractSyntax.{atom, plain}

  import scala.collection.mutable
  val statements = new mutable.Stack[Formula]()

  final val rdf_type = URI(nsuri + "type")

  def parse(e: Elem): Formula = {
    e match {
      case <RDF>{children @ _*}</RDF> if (e.namespace == nsuri) => {
	children foreach {
	  case ne: Elem => parseNodeElement(ne)
	  case _ => // TODO: make sure any intervening text is whitespace
	}
      }
      case _ => parseNodeElement(e)
    }

    val f1 = And(statements.toList)
    val vars = f1.variables() // or keep a list/stack as we go?
    if (vars.isEmpty) f1
    else Exists(vars, f1)
  }

  private def euri(e: Elem) = URI(e.namespace + e.label)

  def parseNodeElement(ne: Elem): Term = {
    val abouts = ne \ ("@{" + nsuri + "}about")
    val subject = if (abouts.isEmpty) BlankNode("e", Some(ne.hashCode)) else {
      URI(abouts.text) }
    
    ne match {
      case <Description>{children @_*}</Description> if (ne.namespace == nsuri)
	=> parseProperties(subject, children)
      case _ => {
	statements.push(atom(subject, rdf_type, euri(ne)))
	parseProperties(subject, ne.child)
      }
    }

    subject
  }

  def parseProperties(subject: Term, children: NodeSeq) {
    children foreach {
      case e: Elem => {
	val objectt = plain(e.text) // TODO: other object syntaxes
	statements.push(atom(subject, euri(e), objectt))
      }
      case _ => // TODO: check that only whitespace goes here.
    }
  }
}

