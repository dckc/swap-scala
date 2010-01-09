
import org.scalacheck._
import Prop._
import Arbitrary.arbitrary

/* i hate import ._, but that's what the docs
 * show and I can't figure out any other way.
 * {Properties, Gen, Arbitrary, Prop} */

/* cf http://code.google.com/p/scalacheck/
 */

import org.w3.swap.NTriples

object ntp extends Properties("NTriples parsing") {

  property ("ground doc") = forAll(genDoc) {
    doc => false
//    doc =>
//      new NTriples().toFormula(doc).variables().isEmpty
  }

  val genConstant = Gen.oneOf("<data:bob>",
			      "<data:dan>",
			      "\"str1\"",
			      "\"str2\"")
  val genVar = for {
    i <- arbitrary[Int]
  } yield "_:v" + i.toString()

  val genLine1 = for {
    s <- genConstant
    p <- genConstant
    o <- genConstant
  } yield s + " " + p + " " + o + ".\n"

  def genDoc = genLine1

}
