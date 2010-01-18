package org.w3.swap

import scala.util.matching.Regex

object StringUtil {

  val escNT = new Regex("""(?x) # x = COMMENTS
			(\\u[0-9A-F]{4})
			|(\\U[0-9A-F]{8})
			|(\\[tnr])
			|(\\""" + "\"" + """)
			|(\\\\)""",
			"h4", "h8", "space", "quote", "backslash")

  val handle = Map[String, (String => String)](
    ("h4" -> {
      case s: String => "" + Integer.parseInt(s.substring(2), 16).toChar }),
    ("h8" -> {
      case s: String => "" + Integer.parseInt(s.substring(2), 16).toChar }),
    ("space" -> {
      case "\\t" => "\t"
      case "\\n" => "\n"
      case "\\r" => "\r"
    }),
    ("quote" -> { case _ => "\"" }),
    ("backslash" -> { case _ => "\\"}) )

  /**
   * Evaluate a (quoted) string expression, as per <a
   * href="http://www.w3.org/TR/2004/REC-rdf-testcases-20040210/#ntriples"
   * >N-Triples spec</a>.
   */
  def dequote(exp: String): String = grind(exp, escNT, handle)

  val encNT = new Regex("""(?x) # x = COMMENTS
			(\\)
			|(""" + "\"" + """)
			|([\t\r\n])
			|([\x00-\x08\x0B-\x0C\x0E-\x0F\x7F-\uFFFF])
			|([^\x00-\uFFFF])
			""",
			"backslash", "quote", "space", "h4", "h8")

  val handle2 = Map[String, (String => String)](
    ("backslash" -> { case _ => "\\\\"}),
    ("quote" -> { case _ => "\\\"" }),
    ("space" -> {
      case "\t" => "\\t"
      case "\n" => "\\n"
      case "\r" => "\\r"
    }),
    ("h4" -> { case s: String =>
      String.format("\\u%04X", s(0).toInt.asInstanceOf[java.lang.Integer]) }),
    ("h8" -> { case s: String =>
      String.format("\\u%08X", s(0).toInt.asInstanceOf[java.lang.Integer]) })
  )


  /**
   * Evaluate a (quoted) string expression, as per <a
   * href="http://www.w3.org/TR/2004/REC-rdf-testcases-20040210/#ntriples"
   * >N-Triples spec</a>.
   */
  def quote(s: String): String = grind(s, encNT, handle2)

  def grind(s: String, exp: Regex, handlers: Map[String, (String=>String)]
	  ): String = {
    exp.findFirstIn(s) match {
      case None => s
      case Some(_) => {
	val buf = new StringBuilder()
	var cur = 0
	for (m <- exp.findAllIn(s).matchData) {
	  buf.append(s.substring(cur, m.start))
	  for ((name, func) <- handlers; x = m.group(name); if x != null) {
	    buf.append(func(x))
	  }
	  cur = m.end
	}
	if (cur < s.length) buf.append(s.substring(cur, s.length))
	buf.toString()
      }
    }
  }
}
