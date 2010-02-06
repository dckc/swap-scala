package org.w3.swap.rdf

import org.w3.swap
import swap.logic.{Formula, Apply, Literal}

import scala.xml
import scala.xml.{Elem, NamespaceBinding, TopScope}

object RDFXMLout{
  val rdfdecl = NamespaceBinding("rdf", Vocabulary.nsuri, TopScope)

  def asxml(g: Graph): Elem = {
    new Elem("rdf", "RDF", xml.Null, rdfdecl,
	     // TODO: noodle on collection api some more (scalaq)
	     new xml.Group(g.arcs.map(arc => asxml(arc)).toSeq) )
  }

  // TODO: real character classes for namestartchar, namechar
  // TODO: ".../mbox_sha1sum" => (".../mbox_sha1", "sum")
  final val splitter = "(.*[^a-zA-Z])([a-zA-Z][a-zA-Z0-9_-]*)".r

  def asxml(arc: Holds): Elem = {

    def id(b: BlankNode) = b match {
      case BlankNode(n, None) => n
      // TODO: ensure that x.toString() consists only of XML name chars
      case BlankNode(n, Some(x)) => n + x.toString()
    }

    def attr1(pfx: String, name: String, value: String) =
      new xml.PrefixedAttribute(pfx, name, value, xml.Null)

    val subjattr = arc.s match {
      case URI(i) => attr1("rdf", "about", i)

      // TODO: resolve doubts about uniqueness of these names
      case v: BlankNode => attr1("rdf", "nodeID", id(v))
    }

    val propElem = arc.p match {
      case URI(splitter(ns, ln)) => {
	val pns = NamespaceBinding("ns0", ns, TopScope)

	arc.o match {
	  case v: BlankNode =>
	    Elem("ns0", ln, attr1("rdf", "nodeID", id(v)), pns)
	  case URI(i) =>
	    Elem("ns0", ln, attr1("rdf", "resource", i), pns)

	  case Literal(s: String) => true
	    Elem("ns0", ln, xml.Null, pns, xml.Text(s))

	  case Apply('text,
		     List(Literal(s: String), Literal(code: Symbol) )) =>
	    Elem("ns0", ln, attr1("xml", "lang", code.name), pns, xml.Text(s))

	  case Apply('data, List(URI(dt), Literal(s: String) )) =>
	    Elem("ns0", ln, attr1("rdf", "datatype", dt), pns, xml.Text(s))

	  case Apply('xml, List(Literal(e: xml.NodeSeq))) =>
	    Elem("ns0", ln, attr1("rdf", "parseType", "Literal"),
		 pns, new xml.Group(e))

	  case _ => throw new Exception("object not well formed" + arc.o)
	}
      }
      case _ => throw new Exception("@@can't split property URI" + arc.p)
    }

    Elem("rdf", "Description", subjattr, rdfdecl, propElem)
  }

}
