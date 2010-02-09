package org.w3.swap.rdf

import org.w3.swap
import swap.logic.{Formula, And, Exists, Term}
import swap.uri.Util.combine

import scala.xml

/**
 * This parser is host-language neutral, so caller must
 * fish base out of HTML head.
 *
 * @See: <a href="http://www.w3.org/TR/rdfa-syntax/"
 * >RDFa in XHTML: Syntax and Processing</a>
 * W3C Recommendation 14 October 2008
 *
 */
object RDFaSyntax{
  // TODO: consider making rdf.AbstractSyntax an interface/trait...
  import AbstractSyntax.{plain, text, data, rdf_type, xml => xmllit }

  def getFormula(e: xml.Elem, base: String): Formula = {
    val f1 = And(getArcs(e, base).iterator.toSeq)
    val vars = f1.variables()
    if (vars.isEmpty) f1
    else Exists(vars, f1)
  }

  // TODO: rename Holds to Arc? just use scala tuple here?
  def getArcs(e: xml.Elem, base: String): Stream[Holds] = {
    walk(e, base, new XMLNameScope(), URI(base), null, Nil, Nil, null)
  }

  /**
   * Walk element recursively, finding arcs (atomic formulas).
   * 
   * based on section <a href="http://www.w3.org/TR/rdfa-syntax/#sec_5.5."
   * >5.5. Sequence</a>
   * 
   *
   * @param subj1: [parent subject] from step 1
   * @param obj1: [parent object] from step 1
   * @param pending1f: propertys of [list of incomplete triples]
   *                   from evaluation context, forward direction
   * @param pending1r: propertys of [list of incomplete triples]
   *                   from evaluation context, reverse direction
   * @param lang1: [language] from step 1
   *
   */
  def walk(e: xml.Elem, base: String, vars: XMLNameScope,
	   subj1: Term, obj1: Term,
	   pending1f: Iterable[Term], pending1r: Iterable[Term],
	   lang1: Symbol): Stream[Holds] = {
    assert(subj1 != null) // with NotNull doesn't seem to work. scalaq?

    // step 2., URI mappings, is taken care of by scala.xml

    // step 3. [current language]
    val lang2 = e \ "@{http://www.w3.org/XML/1998/namespace}lang"
    val lang = {
      if (lang2.isEmpty) lang1
      else if (lang2.text == "") null
      else Symbol(lang2.text.toLowerCase)
    }

    // steps 4 and 5, refactored
    val relterms = CURIE.refN(e, "@rel", true)
    val revterms = CURIE.refN(e, "@rev", true)
    val types = CURIE.refN(e, "@typeof", false)
    val props = CURIE.refN(e, "@property", false)
    val (subj45, objref5, skip) = subjectObject(obj1, e, base, vars,
						(e \ "@rel").isEmpty &&
						(e \ "@rev").isEmpty,
						types, props)

    // step 6. typeof
    val arcs6 = {
      if (subj45 != null)
	types.toStream.map(cls => Holds(subj45, rdf_type, cls))
      else Stream.empty
    }

    // step 7 rel/rev triples
    // HTML grammar guarantees a subject at this point, I think,
    // but in an effort to stay host-language-neutral, let's double-check
    val arcs7 = {
      if (objref5 != null && subj45 != null) {
	(for (p <- relterms.toStream) yield Holds(subj45, p, objref5)) ++
	(for (p <- revterms.toStream) yield Holds(objref5, p, subj45))
      } else Stream.empty
    }

    // step 8 incomplete triples.
    val (objref8, pending8f, pending8r) = {
      if (objref5 == null && !(relterms.isEmpty && revterms.isEmpty))
	(vars.fresh("x8"), relterms, revterms)
      else (objref5, Nil, Nil)
    }

    // step 9 literal object
    val (arcs9, xmlobj) = {
      if (!props.isEmpty) literalObject(subj45, props, lang, e)
      else (Stream.empty, false)
    }

    // step 10 complete incomplete triples.
    val arcs10: Stream[Holds] = if (!skip && subj45 != null) {
      pending1f.toStream.map(Holds(subj1, _, subj45)) ++
      pending1r.toStream.map(Holds(subj45, _, subj1))
    } else Stream.empty

    // step 11. recur
    arcs6 ++ arcs7 ++ arcs9 ++ arcs10 ++ (if (!xmlobj) {
      e.child.toStream.flatMap {
	case c: xml.Elem => {
	  if (skip) walk(c, base, vars, subj1, obj1,
			 pending1f, pending1r, lang)
	  else walk(c, base, vars,
		    if (subj45 != null) subj45 else subj1,
		    (if (objref8 != null) objref8
		     else if (subj45 != null) subj45
		     else subj1),
		    pending8f, pending8r, lang)
	}
	case _ => Stream.empty /* never mind stuff other than elements */
      }
    } else Stream.empty)
  }

  /**
   * steps 4 and 5, refactored
   * @return: new subject, new object ref, skip flag
   */
  def subjectObject(obj1: Term, e: xml.Elem, base: String, vars: XMLNameScope,
		    norel: Boolean,
		    types: Iterable[URI], props: Iterable[URI]
		  ): (Term, Term, Boolean) = {
    val about = CURIE.ref1(e, "@about", base, vars)
    lazy val src = e \ "@src"
    lazy val resource = CURIE.ref1(e, "@resource", base, vars)
    lazy val href = e \ "@href"

    val subj45x = {
      if (!about.isEmpty) about.get
      else if (!src.isEmpty) URI(combine(base, src.text))
      else if (norel && !resource.isEmpty) resource.get
      else if (norel && !href.isEmpty) URI(combine(base, href.text))
      // hmm... host language creeping in here...
      else if (e.label == "head" || e.label == "body") URI(combine(base, ""))
      else if (!types.isEmpty) vars.fresh("x4")
      else null
    }

    val objref5 = (if (!resource.isEmpty) resource.get
		   else if (!href.isEmpty) URI(combine(base, href.text))
		   else null
		 )

    val subj45 = if (subj45x != null) subj45x else obj1
    val skip = norel && (subj45x == null) && props.isEmpty

    return (subj45, objref5, skip)
  }

  /**
   * step 9 literal object
   * side effect: pushes statements
   * @return: (arcs, xmllit) where xmllit is true iff object is XMLLiteral
   */
  def literalObject(subj: Term, props: Iterable[URI], lang: Symbol,
		    e: xml.Elem): (Stream[Holds], Boolean) = {
    val content = e \ "@content"
    val datatype = e \ "@datatype"

    lazy val lex = if(!content.isEmpty) content.text else e.text

    def txt(s: String) = if (lang == null) plain(s) else text(s, lang)

    lazy val alltext = e.child.forall {
      case t: xml.Text => true; case _ => false
    }

    def sayit(obj: Term) = {
      for(p <- props.toStream) yield Holds(subj, p, obj)
    }

    (!datatype.isEmpty, !content.isEmpty) match {
      case (true, _) if datatype.text == "" => (sayit(txt(lex)), false)

      case (true, _) => {
	datatype.text match {
	  case CURIE.parts(p, l) if p != null => {
	    try {
	      val dt = CURIE.expand(p, l, e)

	      if (dt == Vocabulary.XMLLiteral) (sayit(xmllit(e.child)), true)
	      else (sayit(data(lex, URI(dt))), false)
	    } catch {
	      case e: NotDefinedError => (Stream.empty, false)
	    }
	  }
	  /* TODO: update handling of goofy datatype values based on WG
	   * response to 3 Feb comment. */
	  case _ => (Stream.empty, false)
	}
      }

      case (_, true) => (sayit(txt(content.text)), false)
      case (_, _) if alltext => (sayit(txt(e.text)), false)
      case (_, _) if e.child.isEmpty => (sayit(txt("")), false)
      case (_, _) => (sayit(xmllit(e.child)), true)
    }
  }

}


/**
 * There is perhaps a more general notion of CURIE, but this captures
 * only the RDFa-specific notion.
 */

object CURIE {
  import scala.util.matching.Regex

  /*
   * spec says
   *   prefix := NName
   * but then speaks of :p having a "default prefix"
   * so we match the empty prefix here.
   */
  final val parts = new Regex("""^(?:([^:]*)?:)?(.*)$""",
			      "prefix", "reference")
  final val parts2 = new Regex("""^\[(?:([^:]*)?:)?(.*)\]$""",
			       "prefix", "reference")

  /**
   * expand one safe curie or URI reference
   *
   */
  def ref1(e: xml.Elem, attr: String, base: String,
	   vars: XMLNameScope): Option[Term] = {
    val attrval = e \ attr
    if (attrval.isEmpty) None
    else attrval.text match {
      case parts2(p, _) if p == null => None
      case parts2("_", "") => Some(vars.byName("_"))
      // TODO: encode arbitrary strings as XML names
      case parts2("_", l) if l.startsWith("_") => Some(vars.byName(l))
      case parts2("_", l) => Some(vars.byName(l))
      case parts2(p, l) => Some(URI(expand(p, l, e)))
      case ref => Some(URI(combine(base, ref)))
    }
  }

  // 9.3. @rel/@rev attribute values
  def reserved = Array("alternate",
		       "appendix",
		       "bookmark",
		       "cite",
		       "chapter",
		       "contents",
		       "copyright",
		       "first",
		       "glossary",
		       "help",
		       "icon",
		       "index",
		       "last",
		       "license",
		       "meta",
		       "next",
		       "p3pv1",
		       "prev",
		       "role",
		       "section",
		       "stylesheet",
		       "subsection",
		       "start",
		       "top",
		       "up")
  final val xhv = "http://www.w3.org/1999/xhtml/vocab#"

  def refN(e: xml.Elem, attr: String, bare: Boolean): Iterable[URI] = {
    "\\s+".r.split((e \ attr).text) flatMap {
      case token if (bare && reserved.contains(token)) =>
	List(URI(xhv + token))

      case CURIE.parts(p, l) if (p == null) => Nil // foo

      case CURIE.parts(p, l) if (p == "_") => Nil // _:foo

      case CURIE.parts(p, l) if (p == "xml") => Nil // xml:foo

      case CURIE.parts(p, l) =>	try {
	List(URI(CURIE.expand(p, l, e)))
      } catch {
	case e: NotDefinedError => Nil
      }

      case _ => Nil
    }
  }

  def expand(p: String, l: String, e: xml.Elem): String = {
    val ns = if (p == "") xhv else e.getNamespace(p)

    if (ns == null) {
      // TODO: find out if we're supposed to ignore this error.
      throw new NotDefinedError("no such prefix " + p + " on element " + e)
    }
    ns + l
  }
}
