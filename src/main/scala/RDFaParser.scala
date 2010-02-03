package org.w3.swap.rdf

import org.w3.swap
import swap.logic.{Formula, And, Exists, Term}
import swap.uri.Util.combine

import scala.xml.{Elem}

/**
 * This parser is host-language neutral, so caller must
 * fish base out of HTML head.
 *
 * @See: <a href="http://www.w3.org/TR/rdfa-syntax/"
 * >RDFa in XHTML: Syntax and Processing</a>
 * W3C Recommendation 14 October 2008
 * 
 */
class RDFaParser(base: String) {
  import AbstractSyntax.{plain, text, data, rdf_type}

  import scala.collection.mutable
  val statements = new mutable.Stack[Formula]()
  val blank = BlankNode("node", None) // source of fresh variables

  def parse(e: Elem): Formula = {
    walk(e, URI(base), null, null)

    val f1 = And(statements.toList.reverse)
    val vars = f1.variables()
    if (vars.isEmpty) f1
    else Exists(vars, f1)
  }

  def walk(e: Elem, subj1: Term, obj1: Term, lang1: String) {
    assert(subj1 != null) // with NotNull doesn't seem to work. scalaq?

    // TODO: incomplete triples
    val about = e \ "@about"
    // TODO: subj can come from @src, @resource, @href
    val subj = (if (about.isEmpty) subj1
		else URI(CURIE.ref1(about.text, e, base))
	      )

      // TODO: rel *and* property on the same element makes for 2 triples
    val property = e \ "@property"
    val rel = e \ "@rel"
    val rev = e \ "@rev"
    val pred = (if (!property.isEmpty) URI(CURIE.expand(property.text, e))
		else if (!rel.isEmpty) URI(CURIE.expand(rel.text, e))
		else null
	      )

    val href = e \ "@href"
    val content = e \ "@content"
    val lang2 = e \ "@lang"
    val lang = if (lang2.isEmpty) lang1 else lang2.text
    val datatype = e \ "@datatype"
    def string_term(s: String) = (
      if (!datatype.isEmpty) data(s, URI(CURIE.expand(datatype.text, e)))
      else if (lang != null) text(s, lang)
      else plain(s)
    )

    val obj = (if (!href.isEmpty) URI(combine(base, href.text))
	       else if (!content.isEmpty) string_term(content.text)
	       else if (!property.isEmpty) string_term(e.text)
	       else obj1
	     )

    if (pred != null && obj != null) {
      statements.push(Holds(subj, pred, obj))
    }

    val typeof = e \ "@typeof"
    if (!typeof.isEmpty) {
      statements.push(Holds(subj, rdf_type, URI(CURIE.expand(typeof.text, e))))
    }

    // TODO: propagate @href to subject, etc.
    e.child.foreach {
      case c: Elem => walk(c, subj, obj1, lang)
      case _ => /* never mind stuff other than elements */
    }
  }

}

object CURIE {
  import scala.util.matching.Regex

  final val parts = new Regex("""\[?([^:]+):([^\]]+)\]?$""",
			      "prefix", "localname")

  /**
   * expand one safe curie or URI reference
   */
  def ref1(ref: String, e: Elem, base: String): String = {
    if (ref.startsWith("[")) expand(ref, e)
    else combine(base, ref)
  }

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
