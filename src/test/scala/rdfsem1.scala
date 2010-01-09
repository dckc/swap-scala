
import org.scalacheck._
import Prop._
import Arbitrary.arbitrary

/* i hate import ._, but that's what the docs
 * show and I can't figure out any other way.
 * {Properties, Gen, Arbitrary, Prop} */

/* cf http://code.google.com/p/scalacheck/
 */

import org.w3.swap.NTriples
import org.w3.swap.SyntaxError
import org.w3.swap.rdf2004.AbstractSyntax.wellformed

object ntp extends Properties("NTriples parsing") {

  val genSubj = Gen.oneOf("<data:bob>", "<data:dan>",
			  "_:x", "_:y", "_:z")
  val genPred = Gen.oneOf("<data:name>", "<data:title>")

  val genObj = Gen.oneOf("<data:bob>", "<data:dan>",
			 "_:x", "_:y", "_:z",
			 "\"str1\"", "\"str2\"",
			 "\"str1\"@en", "\"str2\"@fn",
			 "\"str1\"^^<data:int>", "\"str2\"^^<data:date>"
		       )

  val gen3 = for {
    s <- genSubj
    p <- genPred
    o <- genObj
  } yield (s, p, o)

  val genLines = for {
    l <- Gen.choose(0, 12)
    v <- Gen.listOfN(l, gen3)
  } yield v

  def mkdoc(lines: List[(String, String, String)]): String = {
    val l = lines.map(t => t._1 + " " + t._2 + " " + t._3 + ".")

    /* scalaQ: no list.join("\n") in the Scala lib? */
    l.foldLeft("")((s1, s2) => s1 + "\n" + s2)
  }

  property("gives well formed formula on good parse") = Prop.forAll(genLines){
    lines => {
      try {
	val f = new NTriples().toFormula(mkdoc(lines))
	wellformed(f)
      } catch {
	case a: SyntaxError => true // bad text syntax; skip it

	case other => throw other // scalaQ: no re-raise syntax?
      }
    }
  }
}
