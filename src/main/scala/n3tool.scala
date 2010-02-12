package org.w3.swap

import java.io.{FileReader, FileNotFoundException}

import org.w3.swap
import swap.webdata.TurtleParser
import swap.rdflogic.RDFLogic
import swap.webdata.{WebData,RDFQ}

object N3Tool {
  val stdout = new java.io.OutputStreamWriter(java.lang.System.out)

  def main(args: Array[String]): Unit = {
    if (args.length >= 2 && args(0) == "--rdf"){
      readRDF(args(1))
    } else if (args.length >= 2 && args(0) == "--rdfa"){
      readRDFa(args(1))
    } else if (args.length == 1) {
      readTurtle(args(0))
    } else {
      println("Usage: n3tool <addr>")
      println("or   : n3tool --rdf <addr>")
      println("or   : n3tool --rdfa <addr>")
      return // TODO: non-0 return value
    }

    println()
    stdout.close()
  }

  def readTurtle(fname: String) {
    val parser = new TurtleParser(fname)
    try {
      val reader = new FileReader(fname)
      val arcs = parser.arcs(parser.parseAll(parser.turtleDoc, reader))
      rdfxml.SimpleSerializer.writeArcsDoc(stdout, arcs)
    } catch {
      // TODO: diagnostics to stderr
      case e: FileNotFoundException => {
	println(e)
	return
      }
    }
  }

  def readRDF(addr: String) {
    val arcs = WebData.loadRDFXML(addr)
    val q = RDFQ.quote(RDFLogic.graphFormula(arcs))

    q.doc.format(72, stdout)
  }

  def readRDFa(addr: String) {
    val arcs = WebData.loadRDFa(addr)
    val q = RDFQ.quote(RDFLogic.graphFormula(arcs))
    q.doc.format(72, stdout)
  }
}
