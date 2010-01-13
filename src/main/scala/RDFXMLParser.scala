package org.w3.swap

import scala.xml.{Elem, NodeSeq, Node}

object RDFSyntax {
  final val nsuri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
}

class RDFXMLParser(base: String) {
  import RDFSyntax.nsuri
  import logicalsyntax.{Formula, Exists, And, Term}
  import rdf2004.{URI, BlankNode}
  import rdf2004.AbstractSyntax.{atom, plain, data, text}

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
  private final val attr_ID = "@{" + nsuri + "}ID"
  private final val attr_about = "@{" + nsuri + "}about"
  private final val attr_resource = "@{" + nsuri + "}resource"
  private final val attr_nodeID = "@{" + nsuri + "}nodeID"
  private final val attr_parseType = "@{" + nsuri + "}parseType"
  private final val attr_datatype = "@{" + nsuri + "}datatype"
  private final val attr_lang = "@{http://www.w3.org/XML/1998/namespace}lang"

  def parseNodeElement(ne: Elem): Term = {
    val about = ne \ attr_about
    val id = ne \ attr_ID
    val nodeID = ne \ attr_nodeID
    val subject = {
      if (about.length > 0) URI(about.text) // @@TODO: relative URI refs
      else if (id.length > 0) URI(base + "#" + id.text)
      else if (nodeID.length > 0) BlankNode(nodeID.text, None)
      else BlankNode("e", Some(ne.hashCode))
    }
    
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
	val res = e \ attr_resource
	val pt = e \ attr_parseType
	val dt = e \ attr_datatype
	val lang = e \ attr_lang
	val elems = e.child.filter(c => c.isInstanceOf[Elem])

	val obj = (!res.isEmpty, pt.text, elems.length) match {
	  case (true, _, _) => URI(base + res.text) // @@TODO: URI.combine
	  case (false, "Resource", _) => {
	    val r = BlankNode("e", Some(e.hashCode()))
	    parseProperties(r, e.child)
	    r
	  }
	  case (false, "Literal", _) => throw new Exception("@@TODO: XML")
	  case (false, "Collection", _) =>
	    throw new Exception("@@TODO: Collection")
	  case (false, "", 1) => parseNodeElement(elems(0).asInstanceOf[Elem])
	  case (false, "", 0) if !dt.isEmpty => data(e.text, URI(dt.text))
	  case (false, "", 0) if !lang.isEmpty => text(e.text, lang.text)
	  case (false, "", 0) => plain(e.text)
	  case _ => 
	    throw new Exception("@@bad object syntax")
	}
	statements.push(atom(subject, euri(e), obj))
      }
      case _ => // TODO: check that only whitespace goes here.
    }
  }
}

