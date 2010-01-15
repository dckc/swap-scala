package org.w3.swap.qa

import scala.util.matching.Regex
import java.text.ParseException

/**
 * Find scala example/test cases and make them executable.
 * Inspired by <a href="http://docs.python.org/library/doctest.html"
 * >python's doctest module</a>.
 * 
 * @author Dan Connolly
 */
object DocTest {
  def main(args: Array[String]) {
    def contents(path: String): String = {
      /* much easier than Java :)
       * see  http://snippets.dzone.com/posts/show/1335 */
      scala.io.Source.fromPath(path).mkString("")
    }

    if (args.length == 2) {
      makeTestSuite(args(0), contents(args(1)))
    } else {
      println("Usage: doctest package.name input_file_name")
    }
  }

  /**
   * Print a test suite by finding examples in source text.
   * @param pkg package of the resulting test suite
   *
   * Resulting test suite extends <a href=
   * "http://www.scalatest.org/getting_started_with_fun_suiteFunSuite"
   * >scalatests' FunSuite</a>.
   */
  def makeTestSuite(pkg: String, source: String) {
    println("package " + pkg)
    println()
    println("import org.scalatest.FunSuite")
    println("import org.scalatest.matchers.ShouldMatchers")
    println()
    println("object DocTestSuite extends FunSuite with ShouldMatchers {")
    println()

    var i = 0
    for (ex <- examples(source)) {
      i += 1
      
      println("  test(\"" + i + " TODO: test names.\") {")
      println("    //@@TODO: what to import?")
      println("    val actual = (")
      for (line <- ex.source.split("\\\n"))
	println("      " + line)
      println("    )")
      println()
      println("    actual.isTypeOf[" + ex.wantType + "] should equal (true) ")
      println("    actual.toString should equal == (" + quote(ex.want) + ")")
      println("  }")
      println()
    }
    println("}")
  }

  def quote(s: String): String = {
    assert(!s.contains("\""), "TODO: quote \" chars")
    val q3 = "\"\"\""
    q3 + s + q3
  }

  case class Example(source: String, wantType: String, want: String)

  /*
   * cribbed from:
   * 
   * Module doctest.
   * Released to the public domain 16-Jan-2001, by Tim Peters (tim@python.org).
   *
   */
  protected val example_re = new Regex("""(?mx: # mx = MULTILINE | COMMENTS
        # Source consists of a PS1 line followed by zero or more PS2 lines.
        (
            (?:^(           [\x20\*]*) scala>    .*)    # PS1 line
            (?:\n           [\x20\*]*      \|    .*)*)  # PS2 lines
        \n?
        # Want consists of any non-blank lines that do not start with PS1.
        (         (?:(?![\x20\*]*$)    # Not a blank line
                     (?![\x20\*]*\|)   # Not a line starting with PS1
                     .*$\n?       # But any other line
                  )*)
        )""", "source", "indent", "want")

  /**
   * Find scala REPL examples. They take the form:
   * scala> 1+2
   * res0: Int = 3
   *
   * Multiple line input works too:
   * scala> (1 +
   *      | 2)
   * res0: Int = 3
   * 
   */
  def examples(s: String): Iterator[Example] = {
    example_re.findAllIn(s).matchData.map(m => {
      val indent = m.group("indent").length
      val source_lines = m.group("source").split("\\\n")
      val want_lines = m.group("want").split("\\\n")

      // TODO: check that each prompt is followed by a space.
      val l = "scala> ".length
      val source = (source_lines.map(sl => sl.substring(indent+l))
		    ).mkString("\n")
      val want3 = (want_lines.map(wl => wl.substring(indent))
		 ).mkString("\n")

      // split res0: Int = 3
      // into Int and 3
      val xtv = want3.split(" = ", 2)
      if (xtv.length == 2) {
	val xt = xtv(0).split(": ", 2)
	if (xt.length == 2) {
	  Example(source, xt(1), xtv(1))
	} else throw new ParseException("expected resN: type = val", 0)
      } else throw new ParseException("expected resN: type = val", 0)
    })
  }
}

object FileUtil {
}


