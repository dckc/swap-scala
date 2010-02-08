package org.w3.swap.rdf

import Vocabulary.nsuri
import AbstractSyntax.rdf_type
import org.w3.swap
import swap.uri.Util.combine

import scala.xml.{Elem, NodeSeq, Node, PrefixedAttribute}

object XMLtoRDF {
  import Vocabulary.nsuri
  import org.w3.swap.logic.{Formula, Exists, And, Term}
  import AbstractSyntax.{plain, data, text, xml}

  def getArcs(e: Elem, base: String): Stream[Holds] = {
    val vars = new XMLNameScope()

    e match {
      case <RDF>{children @ _*}</RDF> if (e.namespace == nsuri) => {
	children.toStream.flatMap {
	  case ne: Elem => {
	    val (t, arcs) = walkNodeElement(ne, base, vars)
	    arcs
	  }

	  // TODO: make sure any intervening text is whitespace
	  case _ => Stream.empty
	}
      }
      case _ => {
	val (t, arcs) = walkNodeElement(e, base, vars)
	arcs
      }
    }
  }

  private def euri(e: Elem) = URI(e.namespace + e.label)
  private final val attr_ID = "@{" + nsuri + "}ID"
  private final val attr_about = "@{" + nsuri + "}about"
  private final val attr_resource = "@{" + nsuri + "}resource"
  private final val attr_nodeID = "@{" + nsuri + "}nodeID"
  private final val attr_parseType = "@{" + nsuri + "}parseType"
  private final val attr_datatype = "@{" + nsuri + "}datatype"
  private final val attr_lang = "@{http://www.w3.org/XML/1998/namespace}lang"

  def walkNodeElement(ne: Elem, base: String,
		       vars: XMLNameScope): (Term, Stream[Holds]) = {
    val about = ne \ attr_about
    val id = ne \ attr_ID
    val nodeID = ne \ attr_nodeID
    val subject = {
      if (about.length > 0) URI(combine(base, about.text))
      else if (id.length > 0) URI(combine(base, "#" + id.text))
      else if (nodeID.length > 0) vars.byName(nodeID.text)
      else vars.fresh("it")
    }
    
    val arcs1 = ne match {
      case <Description>{children @_*}</Description> if (ne.namespace == nsuri)
	=> walkProperties(subject, children, base, vars)
      case _ => {
	Stream.cons(Holds(subject, rdf_type, euri(ne)),
		    walkProperties(subject, ne.child, base, vars))
      }
    }

    val arcs2 = ne.attributes.toStream.flatMap {
      case PrefixedAttribute(ns, local, value, _) 
      if ne.getNamespace(ns) != nsuri =>
	Stream(Holds(subject, URI(ne.getNamespace(ns) + local),
		     plain(value.text)))
      case _ => Stream.empty // never mind. perhaps check for errors.
    }

    (subject, arcs1 ++ arcs2)
  }

  def walkProperties(subject: Term, children: NodeSeq, base: String,
		      vars: XMLNameScope): Stream[Holds] = {
    children.toStream.flatMap {
      case e: Elem => {
	val res = e \ attr_resource
	val nid = e \ attr_nodeID
	val pt = e \ attr_parseType
	val dt = e \ attr_datatype
	val lang = e \ attr_lang // @@TODO: look on ancestors
	val elems = e.child partialMap { case c: Elem => c }

	if(pt.text == "" && elems.length == 1) {
	  val (obj, arcs) = walkNodeElement(elems(0), base, vars)
	  Stream.cons(Holds(subject, euri(e), obj), arcs)
	} else {
	  val obj = (pt.text, elems.length) match {
	    case ("Resource", _) => {
	      val r = vars.fresh("it")
	      walkProperties(r, e.child, base, vars)
	      r
	    }
	    case ("Literal", _) => xml(e.child)
	    case ("Collection", _) => plain("@@TODO: Collection")
	    case ("", 0) if !res.isEmpty => URI(combine(base, res.text))
	    case ("", 0) if !nid.isEmpty => vars.byName(nid.text)
	    case ("", 0) if !dt.isEmpty =>
	      data(e.text, URI(combine(base, dt.text)))
	    case ("", 0) if !lang.isEmpty => text(e.text, Symbol(lang.text))
	    case ("", 0) => plain(e.text)
	    case _ => 
	      throw new Exception("@@bad object syntax")
	  }
	  Stream(Holds(subject, euri(e), obj))
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

