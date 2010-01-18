package org.w3.swap.test

import org.scalacheck._
import Prop._
import Arbitrary.arbitrary

/* i hate import ._, but that's what the docs
 * show and I can't figure out any other way.
 * {Properties, Gen, Arbitrary, Prop} */

/* cf http://code.google.com/p/scalacheck/
 */


import org.w3.swap

object strutil extends Properties("String Utilities") {
  import swap.StringUtil.{dequote, quote}

  def genParts = Gen.oneOf("\"", "\\", arbitrary[String], "")
  def genQuotEsc = for {
    s <- Gen.choose(1, 10)
    parts <- Gen.listOfN(s, genParts)
  } yield parts.mkString("")

  property("escaping backslash") = Prop.forAll((x: String, y: String) =>
    quote(x + "\\" + y) == (quote(x) + "\\\\" + quote(y)) )

  property("quote distributes over +") = Prop.forAll((x: String, y: String) =>
    quote(x + y) == (quote(x) + quote(y)) )

  property("d(q(q(x))) == q(x)") =
    Prop.forAll(genQuotEsc) {
      case s: String => dequote(quote(quote(s))) == quote(s)
    }

  property("dequote(quote(s)) == s for genQuotEsc") =
    Prop.forAll(genQuotEsc) { case s: String => dequote(quote(s)) == s }

  property("dequote(quote(s)) == s for arbitrary s") =
    Prop.forAll((s: String) => dequote(quote(s)) == s )
}
