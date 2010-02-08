package org.w3.swap

import java.io.{FileReader, FileNotFoundException}

import org.w3.swap
import swap.n3.N3Parser
import swap.rdf.RDFXMLParser

object N3Tool {
  def main(args: Array[String]): Unit = {
    if (args.length >= 2 && args(0) == "--rdf"){
      readRDF(args(1))
    } else if (args.length >= 2 && args(0) == "--rdfa"){
      readRDFa(args(1))
    } else if (args.length == 1) {
      readN3(args(0))
    } else {
      println("Usage: n3parse <file>")
      println("or   : n3parse --rdf <sysid>")
      return // TODO: non-0 return value
    }
  }

  def readN3(fname: String) {
    val base = swap.WebData.asURI(fname)
    val parser = new N3Parser(base)
    try {
      val reader = new FileReader(fname)
      val result = parser.parseAll(parser.document, reader)

      result match {
	case parser.Success(f, _ ) => {
	  val stdout = new java.io.OutputStreamWriter(java.lang.System.out)
	  f.quote().writeTo(stdout)
	}
	case parser.Failure(x, y) => {
	  println("N3Parser failure:")
	  println(x)
	  println(y.pos.longString)
	}
	case parser.Error(x, y) => {
	  println("N3Parser error:")
	  println(x)
	  println(y)
	  None
	}
      }
    } catch {
      // TODO: diagnostics to stderr
      case e: FileNotFoundException => {
	println(e)
	return
      }
    }
  }

  def readRDF(addr: String) {
    // TODO: stream triples as they come using a callback/Stream/etc.
    val f = WebData.loadRDFXML(WebData.asURI(addr))
    val g = new rdf.Graph(f)
    val e2 = rdf.RDFXMLout.asxml(g)
    println(e2)
  }

  def readRDFa(addr: String) {
    // TODO: stream triples as they come using a callback/Stream/etc.
    val base = WebData.asURI(addr)
    val doc = scala.xml.XML.load(base)
    val arcs = swap.rdf.RDFaSyntax.getArcs(doc, base)
    val stdout = new java.io.OutputStreamWriter(java.lang.System.out)

    swap.rdf.RDFXMLout.writeArcsDoc(stdout, arcs)
    stdout.close()
  }
}

