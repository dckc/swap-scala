package org.w3.swap

/**
 * The Semantic Web Application Platform (SWAP) supports
 * processing of RDF and N3 data, rules, and (eventually) proofs.
 *
 * This is a re-implementation in scala of the ideas
 * in the <a href="http://www.w3.org/2000/10/swap/">swap in python</a>
 * started in 2000.
 *
 * <h2>Colophon</h2>
 * <blockquote>
 * Thus, there are commonly two different ways to write doc comments
 * -- as API specifications, or as programming guide documentation.
 * <address>
 * <a href="http://java.sun.com/j2se/javadoc/writingdoccomments/"
 * >How to Write Doc Comments for the Javadoc Tool</a>
 * </address>
 * </blockquote>
 *
 * This is a programming guide. It takes the form of comments
 * on a scala object because scaladoc doesn't do <a
 * href="http://java.sun.com/j2se/javadoc/writingdoccomments/#packagecomments"
 * >package-level comments a la javadoc</a>. There's a
 * <a href="https://lampsvn.epfl.ch/trac/scala/ticket/414"
 * >2 year old enhancement request</a>.
 * */
object PackageProgrammingGuide {
  /**
   * N3Tool shows example usage
   *
   * (I'm not sure how to make links to other classes/objects
   * in scaladoc, so I hope this val declaration does it.)
   * */
  val example_usage = N3Tool
}
