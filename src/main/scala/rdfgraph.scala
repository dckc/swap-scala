package org.w3.swap.rdf

/**
 * RDF abstract syntax per <cite><a
 * href="http://www.w3.org/TR/2004/REC-rdf-concepts-20040210/"
 * >Resource Description Framework (RDF):
 * Concepts and Abstract Syntax</a></cite>
 * W3C Recommendation 10 February 2004
 */
trait RDFGraphParts {
  /**
   * The spec calls them triples, but that seems to be muddled terminology.
   * Let's stick to traditional graph theory terminology here;
   * See rdflogic for mapping to logic terminology and semantics.
   *
   * Use Set[Arc], Stream[Arc], etc. as appropriate; note Iterable[Arc]
   * includes both Set and Stream.
   * 
   */
  type Arc = (SubjectNode, Label, Node)

  type Node
  type Literal <: Node
  type SubjectNode <: Node
  type BlankNode <: SubjectNode
  type Label <: SubjectNode
  type LanguageTag = Symbol

}

/**
 * No concrete method for building blankNodes, as it
 * depends on the concrete syntax.
 */
trait RDFNodeBuilder extends RDFGraphParts {
  val rdf_type = uri(Vocabulary.`type`)
  val rdf_nil = uri(Vocabulary.nil)
  val rdf_first = uri(Vocabulary.first)
  val rdf_rest = uri(Vocabulary.rest)

  def uri(i: String): Label
  def plain(s: String, lang: Option[LanguageTag]): Literal
  def typed(s: String, dt: String): Literal
  def xmllit(content: scala.xml.NodeSeq): Literal
}
