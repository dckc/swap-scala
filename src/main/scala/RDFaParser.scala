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
    walk(e, base, new XMLNameScope(), URI(base), null, List(), List(), null)
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
    val lang = if (lang2.isEmpty) lang1 else Symbol(lang2.text.toLowerCase)

    // steps 4 and 5, refactored
    val relterms = CURIE.refN((e \ "@rel").text, e, true)
    val revterms = CURIE.refN((e \ "@rev").text, e, true)
    val types = CURIE.refN((e \ "@typeof").text, e, false)
    val props = CURIE.refN((e \ "@property").text, e, false)
    val (subj45, objref5, skip) = subjectObject(obj1, e, base, vars,
						relterms.isEmpty &&
						revterms.isEmpty,
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
      else (objref5, List(), List())
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
			 pending8f, pending8r, lang)
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
    val about = e \ "@about"
    lazy val src = e \ "@src"
    lazy val resource = e \ "@resource"
    lazy val href = e \ "@href"

    val subj45x = {
      if (!about.isEmpty) CURIE.ref1(about.text, e, base, vars)
      else if (!src.isEmpty) URI(combine(base, src.text))
      else if (norel && !resource.isEmpty)
	CURIE.ref1(resource.text, e, base, vars)
      else if (norel && !href.isEmpty) URI(combine(base, href.text))
      // hmm... host language creeping in here...
      else if (e.label == "head" || e.label == "body") URI(combine(base, ""))
      else if (!types.isEmpty) vars.fresh("x4")
      else null
    }

    val objref5 = (if (!resource.isEmpty) CURIE.ref1(resource.text,
						     e, base, vars)
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
	  case CURIE.parts(p, _, l) if p != null => {
	    val dt = CURIE.expand(p, l, e)

	    if (dt == Vocabulary.XMLLiteral) (sayit(xmllit(e.child)), true)
	    else (sayit(data(lex, URI(dt))), false)
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

object CURIE {
  import scala.util.matching.Regex

  final val parts = new Regex("""^(?:([^:]+)?(:))?([^\]]+)$""",
			      "prefix", "colon", "reference")
  final val parts2 = new Regex("""^\[(?:([^:]+)?(:))?([^\]]+)\]$""",
			       "prefix", "colon", "reference")

  /**
   * expand one safe curie or URI reference
   *
   * scala> ref1("abc", e, "data:", new XMLNameScope())
   * res0: URI = URI("data:abc")
   * 
   * scala> ref1("[abc:def]", e, "data:", new XMLNameScope())
   * res0: URI = URI("http://example/abc#def")
   *
   * scala> ref1("[_:abc]", e, "data:", new XMLNameScope())
   * res0: URI = BlankNode(abc, None)
   * 
   */
  def ref1(ref: String, e: xml.Elem, base: String,
	   vars: XMLNameScope): Term = ref match {
    case parts2(p, _, l) if p == "_" => vars.byName(l)
    case parts2(p, _, l) if p != null => URI(expand(p, l, e))
    case _ => URI(combine(base, ref))
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
  val xhv = "http://www.w3.org/1999/xhtml/vocab#"

  // TODO: check whether _:xxx is allowed in, e.g., @typeof
  def refN(curies: String, e: xml.Elem, bare: Boolean): Iterable[URI] = {
    for {
      CURIE.parts(p, _, l) <- "\\s+".r.split(curies)
      if l != null
      iqual = if (p != null) CURIE.expand(p, l, e) else null
      i = (if (iqual != null) iqual
	   else if (bare && reserved.contains(l)) xhv + l else null
	 )
      if i != null
    } yield URI(i)
  }

  def expand(p: String, l: String, e: xml.Elem): String = {
    assert(p != null)

    val ns = e.getNamespace(p)
    if (ns == null) {
      // TODO: find out if we're supposed to ignore this error.
      throw new NotDefinedError("no such prefix " + p + " on element " + e)
    }
    ns + l
  }
}
