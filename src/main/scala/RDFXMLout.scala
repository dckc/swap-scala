package org.w3.swap.rdfxml

import org.w3.swap
import swap.rdf.Vocabulary
import swap.rdfgraph.RDFGraph
import swap.rdflogic.{TermNode, Name, Plain, Data, XMLLit}

import scala.xml
import scala.xml.{Elem, NamespaceBinding, TopScope}

object SimpleSerializer extends RDFGraph with TermNode {
  type BlankNode = XMLVar

  val rdfdecl = NamespaceBinding("rdf", Vocabulary.nsuri, TopScope)

  final val root = <rdf:RDF xmlns:rdf={Vocabulary.nsuri}>CONTENT</rdf:RDF>

  def writeArcsDoc(w: java.io.Writer, arcs: Iterable[Arc]) {
    val rootTags = root.toString().split("CONTENT")
    w.write(rootTags(0) + "\n")

    arcs foreach { case arc =>
      xml.XML.write(w, asxml(arc), "utf-8", false, null)
      w.write("\n")
    }

    w.write(rootTags(1) + "\n")
  }

  // TODO: real character classes for namestartchar, namechar
  // TODO: ".../mbox_sha1sum" => (".../mbox_sha1", "sum")
  final val splitter = "(.*[^a-zA-Z])([a-zA-Z][a-zA-Z0-9_-]*)".r

  def asxml(arc: Arc): Elem = {

    def attr1(pfx: String, name: String, value: String) =
      new xml.PrefixedAttribute(pfx, name, value, xml.Null)

    val subjattr = arc._1 match {
      case v: XMLVar => attr1("rdf", "nodeID", v.sym.name)

      case Name(i) => attr1("rdf", "about", i)

      // TODO: check wf somewhere. assert/throw here?
      case x => attr1("rdf", "about",
		      "data:*oops-subject*" + x)
    }

    val propElem = arc._2 match {
      case Name(splitter(ns, ln)) => {
	val pns = NamespaceBinding("ns0", ns, TopScope)

	arc._3 match {
	  case v: BlankNode =>
	    Elem("ns0", ln, attr1("rdf", "nodeID", v.sym.name), pns)
	  case Name(i) =>
	    Elem("ns0", ln, attr1("rdf", "resource", i), pns)

	  case Plain(s, None) =>
	    Elem("ns0", ln, xml.Null, pns, xml.Text(s))

	  case Plain(s, Some(code)) =>
	    Elem("ns0", ln, attr1("xml", "lang", code.name),
		 pns, xml.Text(s))

	  case Data(s, Name(dt)) =>
	    Elem("ns0", ln, attr1("rdf", "datatype", dt), pns, xml.Text(s))

	  case XMLLit(e) =>
	    Elem("ns0", ln, attr1("rdf", "parseType", "Literal"),
		 pns, new xml.Group(e))

	  case _ => throw new Exception("object not well formed" + arc._3)
	}
      }
      case _ => throw new Exception("@@can't split property URI" + arc._2)
    }

    Elem("rdf", "Description", subjattr, rdfdecl, propElem)
  }

}
