package org.w3.swap.rdf

import Vocabulary.nsuri
import AbstractSyntax.rdf_type
import org.w3.swap
import swap.uri.Util.combine

import scala.xml.{Elem, NodeSeq, Node, PrefixedAttribute}

class RDFXMLParser(base: String) {
  import Vocabulary.nsuri
  import org.w3.swap.logic.{Formula, Exists, And, Term}
  import AbstractSyntax.{plain, data, text, xml}

  import scala.collection.mutable
  val statements = new mutable.Stack[Formula]()
  val blank = BlankNode("node", None) // source of fresh variables

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

    val f1 = And(statements.toList.reverse)
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
      if (about.length > 0) URI(combine(base, about.text))
      else if (id.length > 0) URI(combine(base, "#" + id.text))
      else if (nodeID.length > 0) BlankNode(nodeID.text, None)
      else blank.fresh()
    }
    
    ne match {
      case <Description>{children @_*}</Description> if (ne.namespace == nsuri)
	=> parseProperties(subject, children)
      case _ => {
	statements.push(Holds(subject, rdf_type, euri(ne)))
	parseProperties(subject, ne.child)
      }
    }

    ne.attributes.foreach {
      case PrefixedAttribute(ns, local, value, _) 
      if ne.getNamespace(ns) != nsuri =>
	statements.push(Holds(subject, URI(ne.getNamespace(ns) + local),
			      plain(value.text)))
      case _ => None // never mind. perhaps check for errors.
    }

    subject
  }

  def parseProperties(subject: Term, children: NodeSeq) {
    children foreach {
      case e: Elem => {
	val res = e \ attr_resource
	val nid = e \ attr_nodeID
	val pt = e \ attr_parseType
	val dt = e \ attr_datatype
	val lang = e \ attr_lang
	val elems = e.child.filter(c => c.isInstanceOf[Elem])

	val obj = (!res.isEmpty, !nid.isEmpty, pt.text, elems.length) match {
	  case (true, _, _, _) => URI(combine(base, res.text))
	  case (false, true, _, _) => BlankNode(nid.text, None)
	  case (false, false, "Resource", _) => {
	    val r = blank.fresh()
	    parseProperties(r, e.child)
	    r
	  }
	  case (false, false, "Literal", _) => xml(e.child)
	  case (false, false, "Collection", _) => plain("@@TODO: Collection")
	  case (false, false, "", 1) =>
	    parseNodeElement(elems(0).asInstanceOf[Elem])
	  case (false, false, "", 0) if !dt.isEmpty =>
	    data(e.text, URI(combine(base, dt.text)))
	  case (false, false, "", 0) if !lang.isEmpty => text(e.text, lang.text)
	  case (false, false, "", 0) => plain(e.text)
	  case _ => 
	    throw new Exception("@@bad object syntax")
	}
	statements.push(Holds(subject, euri(e), obj))
      }
      case _ => // TODO: check that only whitespace goes here.
    }
  }
}

