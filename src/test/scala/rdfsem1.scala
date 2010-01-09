
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
  val genConst = Gen.oneOf("<data:bob>",
			   "<data:dan>",
			   "\"str1\"",
			   "\"str2\"")

  val gen3 = for {
    s <- genConst
    p <- genConst
    o <- genConst
  } yield (s, p, o)

  val genLines = for {
    l <- Gen.choose(3, 15)
    v <- Gen.vectorOf(l, gen3)
  } yield v

  property("Nilsson - str - 3tup") = Prop.forAll(genLines){
    lines => {
      val l = lines.map(t => t._1 + " " + t._2 + " " + t._3 + ".")
      /* no list.join("\n") in the Scala lib? */
      val doc = l.foldLeft("")((s1, s2) => s1 + "\n" + s2)
      // println(doc)
      true
    }
  }

  /*
  property("explore") = forAll((s: String) => false )

  implicit val arbString = Arbitrary(genConstant)

  */

/*********
  property ("ground doc") = forAll(genDoc) {
    doc => false
//    doc =>
//      new NTriples().toFormula(doc).variables().isEmpty
  }

  val genVar = for {
    i <- arbitrary[Int]
  } yield "_:v" + i.toString()


  def genDoc = genLine1
********/
}
