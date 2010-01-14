package org.w3.swap.qa

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

  case class Example(code: String, output: String)

  /*
   * cribbed from:
   * 
   * Module doctest.
   * Released to the public domain 16-Jan-2001, by Tim Peters (tim@python.org).
   *
   * What a pain that java/scala doesn't do re.VERBOSE. Oops! It does!
   * http://java.sun.com/javase/6/docs/api/java/util/regex/Pattern.html#COMMENTS
   * TODO: revert this regex back to VERBOSE/COMMENTS style.
   */
  val example_re = (
    """((?:(^[ \*]*)scala> .*)(?:\n[ \*]*| .*))""" +
    """((?:(?![ \*]*$)(?![ \*]*scala>).*$\n?))""").r

  /**
   * Find scala REPL examples. They take the form:
   * scala> 1+2
   * res0: Int = 3
   *
   * Multiple line input works too:
   * scala> (1 +
   *      | 2)
   * res0: Int = 3
   */
  def examples(s: String): Iterator[Example] = {
    example_re.findAllIn(s).matchData.map(m => Example(m.group(1), m.group(2)))
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


