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
class RDFaParser(base: String) {
  import AbstractSyntax.{plain, text, data, rdf_type, xml => xmllit }

  import scala.collection.mutable
  val statements = new mutable.Stack[Formula]()
  val blank = BlankNode("node", None) // source of fresh variables

  def parse(e: xml.Elem): Formula = {
    walk(e, URI(base), null, List(), List(), null)

    val f1 = And(statements.toList.reverse)
    val vars = f1.variables()
    if (vars.isEmpty) f1
    else Exists(vars, f1)
  }

  /**
   * walk element recursively, producing triples
   * 
   * based on 5.5. Sequence
   * http://www.w3.org/TR/rdfa-syntax/#sec_5.5.
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
  def walk(e: xml.Elem, subj1: Term, obj1: Term,
	   pending1f: Iterable[Term], pending1r: Iterable[Term],
	   lang1: Symbol) {
    assert(subj1 != null) // with NotNull doesn't seem to work. scalaq?

    // step 2., URI mappings, is taken care of by scala.xml

    // step 3. [current language]
    val lang2 = e \ "@{http://www.w3.org/XML/1998/namespace}lang"
    val lang = if (lang2.isEmpty) lang1 else Symbol(lang2.text.toLowerCase)

    // steps 4 and 5, refactored
    val rel = e \ "@rel"
    val rev = e \ "@rev"
    val typeof = e \ "@typeof"
    val (subj45, objref5, skip) = subjectObject(obj1, e, rel, rev, typeof)

    // step 6. typeof
    if (subj45 != null) {
      for (cls <- CURIE.refN(typeof.text, e, false)) {
	statements.push(Holds(subj45, rdf_type, cls))
      }
    }

    // step 7 rel/rev triples
    // HTML grammar guarantees a subject at this point, I think,
    // but in an effort to stay host-language-neutral, let's double-check
    val relterms = CURIE.refN(rel.text, e, true)
    val revterms = CURIE.refN(rev.text, e, true)
    if (objref5 != null && subj45 != null) {
      for (p <- relterms)
	statements.push(Holds(subj45, p, objref5))
      for (p <- revterms)
	statements.push(Holds(objref5, p, subj45))
    }

    // step 8 incomplete triples.
    val (objref8, pending8f, pending8r) = (
      if (objref5 == null && (!relterms.isEmpty || !revterms.isEmpty))
	(blank.fresh(), relterms, revterms)
      else (objref5, List(), List()) )

    // step 9 literal object
      // TODO: fix to handle tc54 with property="dc:creator dc:publisher"
    val xmlobj = (e \ "@property").text match {
      case CURIE.parts(p, _, l) if p != null =>
	literalObject(subj45, URI(CURIE.expand(p, l, e)), lang, e)
      case _ => false
    }

    // step 10 complete incomplete triples.
    if (!skip && subj45 != null) {
      for(p <- pending1f) statements.push(Holds(subj1, p, subj45))
      for(p <- pending1r) statements.push(Holds(subj45, p, subj1))
    }

    // step 11. recur
    if (!xmlobj) {
      e.child.foreach {
	case c: xml.Elem => {
	  if (skip) walk(c, subj1, obj1, pending8f, pending8r, lang)
	  else walk(c,
		    if (subj45 != null) subj45 else subj1,
		    (if (objref8 != null) objref8
		     else if (subj45 != null) subj45
		     else subj1),
		    pending8f, pending8r, lang)
	}
	case _ => /* never mind stuff other than elements */
      }
    }
  }

  /**
   * steps 4 and 5, refactored
   * @return: new subject, new object ref, skip flag
   */
  def subjectObject(obj1: Term, e: xml.Elem,
		    rel: xml.NodeSeq, rev: xml.NodeSeq,
		    typeof: xml.NodeSeq
		  ): (Term, Term, Boolean) = {
    val about = e \ "@about"
    lazy val src = e \ "@src"
    lazy val resource = e \ "@resource"
    lazy val href = e \ "@href"

    val norel = rev.isEmpty && rel.isEmpty

    val subj45x = {
      if (!about.isEmpty) CURIE.ref1(about.text, e, base)
      else if (!src.isEmpty) URI(combine(base, src.text))
      else if (norel && !resource.isEmpty) CURIE.ref1(resource.text, e, base)
      else if (norel && !href.isEmpty) URI(combine(base, href.text))
      // hmm... host language creeping in here...
      else if (e.label == "head" || e.label == "body") URI(combine(base, ""))
      else if (!typeof.isEmpty) blank.fresh()
      else null
    }

    val objref5 = (if (!resource.isEmpty) CURIE.ref1(resource.text, e, base)
		   else if (!href.isEmpty) URI(combine(base, href.text))
		   else null
		 )

    val subj45 = if (subj45x != null) subj45x else obj1
    val skip = norel && (subj45x == null)

    return (subj45, objref5, skip)
  }

  /**
   * step 9 literal object
   * side effect: pushes statements
   * @return: true iff object is XMLLiteral
   */
  def literalObject(subj: Term, pred: URI, lang: Symbol, e: xml.Elem
		  ): Boolean = {
    val content = e \ "@content"
    val datatype = e \ "@datatype"

    lazy val lex = if(!content.isEmpty) content.text else e.text

    def txt(s: String) = if (lang == null) plain(s) else text(s, lang)

    lazy val alltext = e.child.forall {
      case t: xml.Text => true; case _ => false
    }

    def sayit(obj: Term) = statements.push(Holds(subj, pred, obj))

    (!datatype.isEmpty, !content.isEmpty) match {
      case (true, _) if datatype.text == "" => {
	statements.push(Holds(subj, pred, txt(lex)))
	false
      }

      case (true, _) => {
	datatype.text match {
	  case CURIE.parts(p, _, l) if p != null => {
	    val dt = CURIE.expand(p, l, e)

	    if (dt == Vocabulary.XMLLiteral) {
	      sayit(xmllit(e.child))
	      true
	    } else {
	      sayit(data(lex, URI(dt)))
	      false
	    }
	  }
	  /* TODO: update handling of goofy datatype values based on WG
	   * response to 3 Feb comment. */
	  case _ => false
	}
      }

      case (_, true) => sayit(txt(content.text)); false
      case (_, _) if alltext => sayit(txt(e.text)); false
      case (_, _) if e.child.isEmpty => sayit(txt("")); false
      case (_, _) => sayit(xmllit(e.child)); true
    }
  }


  /**
   * local, i.e. non-recursive processing.
   *
   * We'll handle the case of [skip element] set to 'false' here.
   */
  def local(e: xml.Elem) {
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
   * scala> ref1("abc", e, "data:")
   * res0: URI = URI("data:abc")
   * 
   * scala> ref1("[abc:def]", e, "data:")
   * res0: URI = URI("http://example/abc#def")
   *
   * scala> ref1("[_:abc]", e, "data:")
   * res0: URI = BlankNode(abc, None)
   * 
   */
  def ref1(ref: String, e: xml.Elem, base: String): Term = ref match {
    case parts2(p, _, l) if p == "_" => BlankNode(l, None)
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
