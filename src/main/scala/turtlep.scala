package org.w3.swap

import org.w3.swap
import swap.logic.{Formula, Term}

/**
 * TurtleParser is like an N3 parser but it uses Holds rather than NotNil
 * TODO: factor out a real turtle parser rather than using swap.n3
 */ 
class TurtleParser(override val baseURI: String) extends
swap.n3.TextRDF(baseURI) {
  def mkstatement(s: Term, p: Term, o: Term): Formula = {
    swap.rdf.Holds(s, p, o)
  }
}

