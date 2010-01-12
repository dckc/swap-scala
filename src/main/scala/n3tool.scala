package org.w3.swap

import java.io.FileReader

object N3Tool {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("Usage: n3parse <file>")
      return
    }

    val base = "data:@@" // TODO: learn how to get the directory of a file
    val parser = new N3Parser(base)
    val reader = new FileReader(args(0))
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
  }
}

