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
import swap.rdf.AbstractSyntax.wellformed

object ent extends Properties("RDF 2004 Entailment") {
  import swap.logic.{Formula, And}
  import swap.rdf.{URI, BlankNode}
  import swap.rdf.AbstractSyntax.{plain, text, data, checkterm, add}
  import swap.rdf.Semantics.entails

  val genVar = for {
    n <- Gen.choose(1, 5)
  } yield BlankNode("_:v", Some(n))

/* pruning... having trouble finding interesting cases for transitivity.
  val genURI = for {
    scheme <- Gen.oneOf("http:", "data:", "ftp:", "mailto:")
    auth <- Gen.oneOf("//x.example", "//y.example", "//z.example", "")
    path <- Gen.oneOf("/a", "bob@example")
    frag <- Gen.oneOf("", "#date")
  } yield URI("data:" + path + frag)
*/
  val genURI = for {
    s <- Gen.oneOf("dan", "bob", "Texas")
  } yield URI("data:" + s)

  val genLiteral = Gen.oneOf(plain("abc"),
			     data("2006-01-01", URI("<data:#date>")), 
			     text("chat", "fr") )

  val genSPO = for {
    s <- Gen.oneOf(genVar, genURI)
    p <- genURI
    //o <- Gen.oneOf(genVar, genURI, genLiteral)
    o <- Gen.oneOf(genVar, genURI)
  } yield (s, p, o)

  val genGraph = for {
    size <- Gen.frequency(
      (0, 0),
      (10, 1),
      (5, 2),
      (2, 3),
      (1, 4),
      (1, 5),
      (1, 6)
      /*
      */
    )
    arcs <- Gen.listOfN(size, genSPO)
  } yield arcs.foldLeft(And(List()): Formula)(
    (f, spo) => add(f, spo._1, spo._2, spo._3) )

  implicit val arbf: Arbitrary[Formula] = Arbitrary(genGraph)

  property("graphs are well formed") = Prop.forAll((g: Formula) =>
    wellformed(g)
  )

  /********
  val genFG = for {
    f <- genGraph
    g <- genGraph
  } yield (f, g)

  val genEntailment = genFG suchThat (fg => entails(fg._1, fg._2) )

  val genFGH = for {
    fg <- genEntailment
    h <- genGraph
  } yield (fg._1, fg._2, h)
  ********* */

  property("entailment is transitive") =
    Prop.forAll( (f: Formula, g: Formula, h: Formula) =>
      entails(f, g) ==> {
	/*
	println("f |= g")
	println (f)
	println (g)
	*/ 
	entails(g, h) ==> {
	  /*
	  println("... and g |= h. does f |= h?")
	  println (h)
	  */
	  entails(f, h)
	}
      }
   )
}

object ntp extends Properties("NTriples parsing") {
  import swap.ntriples.NTriplesParser

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

    l.mkString("", "\n", "\n")
  }

  property("gives well formed formula on good parse") = Prop.forAll(genLines){
    lines => {
      val f = new NTriplesParser().toFormula(mkdoc(lines))
      wellformed(f)
    }
  }
}
