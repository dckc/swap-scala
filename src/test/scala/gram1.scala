package org.w3.swap.test

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import org.w3.swap

class Gram1 extends Spec with ShouldMatchers {
  import swap.grammar.{XMLName, EBNF, Concat, Lit, CharClass}

  val rules = XMLName.rules
  describe("Formal Grammar Notation Parser") {
    it("should parse the Name productions") {
      rules.map(_.name) should equal ( List('NameStartChar, 'NameChar, 'Name) )
    }
  }

  val g = EBNF.ruleMap(rules)
  describe("EBNF to Regex converter") {
    it("should handle the Name productions") {
      (EBNF.regex(g('Name), g) match {
	case Some(Concat(ei)) => ei
	case _ => Nil
      }).length should equal ( 2 )
    }
  }

  describe("Regex simplifier") {
    it("should handle NameStartChar") {
      ((EBNF.regex(g('NameStartChar), g) match {
	case Some(e) => EBNF.simplify(e)
	case _ => Lit("fail")
      }) match {
	case CharClass(hd :: tl) => hd
	case _ => ('f', 'l')
      }) should equal ((':', ':'))
    }
  }
}


      
