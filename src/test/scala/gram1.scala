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

  describe("Regex simplifier") {
    it("should handle NameChar") {
      ((EBNF.regex(g('NameStartChar), g) match {
	case Some(e) => EBNF.simplify(e)
	case _ => Lit("fail")
      }) match {
	case CharClass(hd :: tl) => hd
	case _ => ('f', 'l')
      }) should equal ((':', ':'))
    }
  }

  describe("Regex serializer") {
    val exp = EBNF.regex(g('NameStartChar), g) match {
      case Some(e) => EBNF.re(EBNF.simplify(e))
      case _ => "fail"
    }

    it("should handle NameStartChar") {
      exp should equal ("[:A-Z_a-z\\xc0-\\xd6\\xd8-\\xf6\\xf8-\\u02ff\\u0370-\\u037d\\u037f-\\u1fff\\u200c-\\u200d\\u2070-\\u218f\\u2c00-\\u2fef\\u3001-\\ud7ff\\uf900-\\ufdcf\\ufdf0-\\ufffd\\uffff-\\uffff]")
    }

    it("should give strings that work as regexes") {
      "_".matches(exp) should equal(true)
    }

    val exp2 = EBNF.regex(g('NameChar), g) match {
      case Some(e) => EBNF.re(EBNF.simplify(e))
      case _ => "fail"
    }

    it("should handle NameChar") {
      exp2 should equal ("[:A-Z_a-z\\xc0-\\xd6\\xd8-\\xf6\\xf8-\\u02ff\\u0370-\\u037d\\u037f-\\u1fff\\u200c-\\u200d\\u2070-\\u218f\\u2c00-\\u2fef\\u3001-\\ud7ff\\uf900-\\ufdcf\\ufdf0-\\ufffd\\uffff-\\uffff\\x2d\\x2e0-9\\xb7\\u0300-\\u036f\\u203f-\\u2040]")
    }

    it("should give working regex for NameChar") {
      "0".matches(exp2) should equal(true)
    }

  }

}
