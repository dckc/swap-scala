package org.w3.swap.uri

/**
 * Path operations on URIs.
 * 
 *
 * References:
 * 
 * <cite><a href="http://www.ietf.org/rfc/rfc2396.txt"
 * >Uniform Resource Identifiers (URI): Generic Syntax</a></cite>
 * 
 * TODO: there's a newer URI RFC
 * <cite><a href="http://www.w3.org/DesignIssues/Model.html"
 * >The Web Model: Information hiding and URI syntax (Jan 98)</a></cite>
 *
 * <a href="http://lists.w3.org/Archives/Public/uri/2001Aug/0021.html"
 * >URI API design [was: URI Test Suite] Dan Connolly (Sun, Aug 12 2001)</a>
 * */
object Util {
  def combine(base: String, ref: String): String = {
    /* syntactic operations on URIs shouldn't be all intertwingled
     * with network operations like GET and POST, but for now,
     * we'll just use the java.net.URL implementation. */
    import java.net.URL
    new URL(new URL(base), ref).toString()
  }
}
