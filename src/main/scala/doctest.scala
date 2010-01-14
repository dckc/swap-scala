package org.w3.swap.qa

import scala.util.matching.Regex
import java.io.{File, BufferedInputStream, FileInputStream}


/**
 * Find scala example/test cases and make them executable.
 * Inspired by <a href="http://docs.python.org/library/doctest.html"
 * >python's doctest module</a>.
 * 
 * @author Dan Connolly
 */
object DocTest {
  def main(args: Array[String]): Unit = {
    import FileUtil.contents

    println("@@TODO: something about scope/import")
    println("@@TODO: find part of scala REPL that does printing")
    for (ex <- examples(contents(args(0)))) {
      println(ex)
      println("@@TODO: convert ex to scala code")
    }
  }

  case class Example(source: String, want: String)

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
      val want = (want_lines.map(wl => wl.substring(indent))
		).mkString("\n")
      
      Example(source, want)
    })
  }
}

object FileUtil {
  def contents(path: String): String = {
    // based on http://snippets.dzone.com/posts/show/1335
    
    val size = (new File(path)).length().asInstanceOf[Int]
    val buffer = new Array[Byte](size);
    val f = new BufferedInputStream(new FileInputStream(path));
    f.read(buffer);
    return new String(buffer);
  }
}


