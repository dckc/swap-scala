package org.w3.swap.rdfxml

import scala.xml.{Elem, NodeSeq, Node, PrefixedAttribute}

import org.w3.swap
import swap.rdf.Vocabulary
import swap.uri.Util.combine
import swap.rdf.RDFNodeBuilder

/**
 * BlankNodes are built from XML names, but their type is still abstract..
 */
trait RDFXMLNodeBuilder extends RDFNodeBuilder {
  def fresh(hint: String): BlankNode
  def byName(name: String): BlankNode
}


abstract class XMLtoRDF extends RDFXMLNodeBuilder {
  import Vocabulary.nsuri

  def getArcs(e: Elem, base: String): Stream[Arc] = {
    e match {
      case <RDF>{children @ _*}</RDF> if (e.namespace == nsuri) => {
	children.toStream.flatMap {
	  case ne: Elem => {
	    val (t, arcs) = walkNodeElement(ne, base)
	    arcs
	  }

	  // TODO: make sure any intervening text is whitespace
	  case _ => Stream.empty
	}
      }
      case _ => {
	val (t, arcs) = walkNodeElement(e, base)
	arcs
      }
    }
  }

  private def euri(e: Elem) = uri(e.namespace + e.label)
  private final val attr_ID = "@{" + nsuri + "}ID"
  private final val attr_about = "@{" + nsuri + "}about"
  private final val attr_resource = "@{" + nsuri + "}resource"
  private final val attr_nodeID = "@{" + nsuri + "}nodeID"
  private final val attr_parseType = "@{" + nsuri + "}parseType"
  private final val attr_datatype = "@{" + nsuri + "}datatype"
  private final val attr_lang = "@{http://www.w3.org/XML/1998/namespace}lang"

  def walkNodeElement(ne: Elem, base: String): (SubjectNode, Stream[Arc]) = {
    val about = ne \ attr_about
    val id = ne \ attr_ID
    val nodeID = ne \ attr_nodeID
    val subject = {
      if (about.length > 0) uri(combine(base, about.text))
      else if (id.length > 0) uri(combine(base, "#" + id.text))
      else if (nodeID.length > 0) byName(nodeID.text)
      else fresh("it")
    }
    
    val arcs1 = ne match {
      case <Description>{children @_*}</Description> if (ne.namespace == nsuri)
	=> walkProperties(subject, children, base)
      case _ => {
	Stream.cons((subject, rdf_type, euri(ne)),
		    walkProperties(subject, ne.child, base))
      }
    }

    val arcs2 = ne.attributes.toStream.flatMap {
      case PrefixedAttribute(ns, local, value, _) 
      if ne.getNamespace(ns) != nsuri =>
	Stream((subject, uri(ne.getNamespace(ns) + local),
		plain(value.text, None)))
      case _ => Stream.empty // never mind. perhaps check for errors.
    }

    (subject, arcs1 ++ arcs2)
  }

  def walkProperties(subject: SubjectNode, children: NodeSeq,
		     base: String): Stream[Arc] = {
    children.toStream.flatMap {
      case e: Elem => {
	val res = e \ attr_resource
	val nid = e \ attr_nodeID
	val pt = e \ attr_parseType
	val dt = e \ attr_datatype
	val lang = e \ attr_lang // @@TODO: look on ancestors
	val elems = e.child partialMap { case c: Elem => c }

	if(pt.text == "" && elems.length == 1) {
	  val (obj, arcs) = walkNodeElement(elems(0), base)
	  Stream.cons((subject, euri(e), obj), arcs)
	} else {
	  val obj = (pt.text, elems.length) match {
	    case ("Resource", _) => {
	      val r = fresh("it")
	      walkProperties(r, e.child, base)
	      r
	    }
	    case ("Literal", _) => xmllit(e.child)
	    case ("Collection", _) => plain("@@TODO: Collection", None)
	    case ("", 0) if !res.isEmpty => uri(combine(base, res.text))
	    case ("", 0) if !nid.isEmpty => byName(nid.text)
	    case ("", 0) if !dt.isEmpty =>
	      typed(e.text, combine(base, dt.text))
	    case ("", 0) if !lang.isEmpty =>
	      plain(e.text, Some(Symbol(lang.text.toLowerCase)))
	    case ("", 0) => plain(e.text, None)
	    case _ => 
	      throw new Exception("@@bad object syntax")
	  }
	  Stream((subject, euri(e), obj))
	}

	/* TODO: 2.12 Omitting Nodes: Property Attributes on an
	 * empty Property Element. (ugh. maybe not.)
	 */
      }

      // TODO: check that only whitespace goes here.
      case _ => Stream.empty
    }
  }
}

