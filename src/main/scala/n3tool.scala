package org.w3.swap

import java.io.{FileReader, FileNotFoundException}

object N3Tool {
  def main(args: Array[String]): Unit = {
    if (args.length >= 2 && args(0) == "--rdf"){
      readRDF(args(1))
    } else if (args.length == 1) {
      readN3(args(0))
    } else {
      println("Usage: n3parse <file>")
      println("or   : n3parse --rdf <sysid>")
      return // TODO: non-0 return value
    }
  }

  def readN3(fname: String) {
    val base = "data:@@" // TODO: learn how to get the directory of a file
    val parser = new N3Parser(base)
    try {
      val reader = new FileReader(fname)
      val result = parser.parseAll(parser.document, reader)

      result match {
	case parser.Success(f, _ ) => println(f.quote().print())
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
    import scala.xml.XML
    import org.w3.swap.RDFXMLParser

    val e = XML.load(addr)
    val p = new RDFXMLParser(addr) // @@TODO: absolutize base
    println(p.parse(e).quote().print())
  }
}

