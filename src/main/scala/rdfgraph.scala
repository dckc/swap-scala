package org.w3.swap.rdfgraph

/**
 * RDF abstract syntax per 2004 specs.
 */
trait RDFGraph {
  /* spec calls them triples, but that seems to be muddled terminology;
   * Let's stick to either traditional graph theory terminology
   * or traditional logic terminology.
   */

  type Node <: AnyRef
  type Literal <: Node
  type SubjectNode <: Node
  type BlankNode <: SubjectNode
  type Label <: SubjectNode

  type Arc = (SubjectNode, Label, Node)


  //TODO: RDFGraph is sort of a misnomer without this, but it's getting
  // in the way
  //def arcs(): Iterable[Arc]

  def uri(i: String): Label
  type LanguageTag = Symbol
  def plain(s: String, lang: Option[LanguageTag]): Literal
  def typed(s: String, dt: String): Literal
  def xmllit(content: scala.xml.NodeSeq): Literal
}
