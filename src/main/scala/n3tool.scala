package org.w3.swap

import java.io.{FileReader, FileNotFoundException}

import org.w3.swap
import swap.webdata.TurtleParser
import swap.rdflogic.{RDFLogic}
import swap.webdata.{WebData,RDFQ}

object N3Tool {
  def main(args: Array[String]): Unit = {
    ToolJob.parseArgs(args) match {
      case ToolJob.Success(job, _) => job()

      case lose => {
	System.err.println(lose)
	System.exit(1)
      }
    }
  }

  /* TODO: integrate functionality of these methods into parser below

  def readTurtle(addr: String) {
    try {
      val arcs = WebData.loadTurtle(addr)
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
    val arcs = WebData.loadData(WebData.web0, addr, WebData.RDFXML)
    val q = RDFQ.quote(RDFLogic.graphFormula(arcs))

    q.doc.format(72, stdout)
  }

  def readRDFa(addr: String) {
    val arcs = WebData.loadData(WebData.web0, addr, WebData.RDFa_types)
    val q = RDFQ.quote(RDFLogic.graphFormula(arcs))
    q.doc.format(72, stdout)
  }
  */
}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader
import scala.util.parsing.input.Position

/**
 * cribbed from http://code.google.com/p/scalacheck/source/browse/trunk/src/org/scalacheck/util/CmdLineParser.scala?r=358
 * New BSD License
 */
class ArgsReader(args: Array[String], i: Int) extends Reader[String] {
  val pos = new Position {
    val column = args.slice(0, i).foldLeft(1)(_ + _.length + 1)
    val line = 1
    val lineContents = args.mkString(" ")
  }
  override def atEnd = {
    i >= args.length
  }
  override def first = if(atEnd) null else args(i)
  override def rest = if(atEnd) this else new ArgsReader(args, i+1)
}

object ToolJob extends Parsers {
  import swap.webdata.WebData.{loadData, web0}
  import swap.rdfxml.SimpleSerializer.writeArcsDoc

  type Elem = String

  type Arc = RDFLogic.Arc

  val stdout = new java.io.PrintWriter(java.lang.System.out)

  def job: Parser[() => Unit] = rep(input) ^^ {
    inputs => {
      {
	() => {
	  val accept = List(WebData.TURTLE, WebData.RDFXML,
			    WebData.XHTML).mkString(", ")
	  val arcs = inputs.flatMap { load => load(accept) }
	  writeArcsDoc(stdout, arcs)
	  stdout.close()
	}
      }
    }
  }

  def input: Parser[(String) => Stream[Arc]] = elem("address",
						    { a => a != null }) ^^ {
    case addr => {
      { mt => loadData(web0, addr, mt) }
    }
  }
  
  def parseArgs(args: Array[String]) = phrase(job)(new ArgsReader(args, 0))
}
