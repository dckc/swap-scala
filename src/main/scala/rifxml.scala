package org.w3.swap.rifxml

import scala.xml

import org.w3.swap
import swap.rif.RIFBuilder
import swap.rif.Vocabulary.ns

abstract class RIFBLDXML extends RIFBuilder {

  def rif_document(e: xml.Elem, base: String): Formula = {
    if (!(e.label == "Document" && e.namespace == ns)){
      throw new Exception("excpected rif:Document; got: " + e.label)
    }

    val imports = (e \ "directive" \ "Import") map {
      i => {
	val profile = i \ "profile" match {
	  case x if x.isEmpty => None
	  case x => Some(x.text)
	}
	((i \ "location").text, profile)
      }
    }

    // TODO: base
    // TODO: prefixes

    val payload = e \ "payload" \ "Group" match {
      case g if g.isEmpty => None
      case g => Some(group_elt(g))
    }
    document(None, Nil, imports, payload)
  }

  def group_elt(g: xml.NodeSeq): Formula = {
    //@@assert(g.label == "Group")

    val f = g \ "_" map {
      case <sentence>{_}<Subclass>{ ts @ _*}</Subclass>{_}</sentence> =>
	subclass_term(baseterm_elt(ts \\ "sub"),
		      baseterm_elt(ts \\ "super"))

      case <sentence>{_}<Member>{ ts @ _*}</Member>{_}</sentence> =>
	membership_term(baseterm_elt(ts \\ "instance"),
			baseterm_elt(ts \\ "class"))

      // TODO: other...
    }

    group(f)
  }

  def baseterm_elt(e: xml.NodeSeq): BaseTerm = {
    e \\ "Const" match {
      case x if x.isEmpty => throw new Exception("TODO: term: " + e)
      case c =>
	data(c.text, (c \ "@type").text)
    }
  }
}
