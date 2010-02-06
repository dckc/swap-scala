package org.w3.swap.webapp

/*
 * Front-side-of-a-page servlet docs seem kinda hard to find.
 * This seemed about my speed:
 * 
 * Step, a scala web picoframework
 * from gabriele renzi April 12th, 2009
 * http://www.riffraff.info/2009/4/11/step-a-scala-web-picoframework
 *
 * http://en.wikipedia.org/wiki/Java_Servlet
 * http://java.sun.com/j2ee/tutorial/1_3-fcs/doc/Servlets.html
 */

import scala.xml.XML

import javax.servlet.http.{HttpServlet,
			   HttpServletRequest, HttpServletResponse}

import org.w3.swap
import swap.WebData

class HelloWorld extends HttpServlet {
  override def doGet(request: HttpServletRequest,
		     response: HttpServletResponse) {
    response.setContentType("text/html")
    response.getWriter.println("" + <p>it's <em>alive!</em></p>)
  }
}

class RDFaExtractor extends HttpServlet {

  /**
   * This provides a scala analog to python's try/except/else
   */
  private def maybe[A](a: => A) = try { Right(a) } catch { case e => Left(e) }

  override def doGet(request: HttpServletRequest,
		     response: HttpServletResponse) {
    val addr = request.getParameter("addr")

    if (addr.startsWith("http://")){
      maybe { XML.load(addr) } match {
	case Right(doc) => {
	  val p = new swap.rdf.RDFaParser(addr)
	  val g = new swap.rdf.Graph(p.parse(doc))
	  val e2 = swap.rdf.RDFXMLout.asxml(g)
	
	  response.setContentType("application/rdf+xml")
	  response.getWriter.println("" + e2)
	}
	case Left(e: java.lang.IllegalArgumentException) =>
	  response.sendError(HttpServletResponse.SC_BAD_REQUEST,
			     "bad address? " + addr)
	case Left(e: java.io.IOException) =>
	  response.sendError(HttpServletResponse.SC_BAD_REQUEST,
			     "trouble loading from " + addr)

	case Left(e: org.xml.sax.SAXParseException) =>
	  response.sendError(HttpServletResponse.SC_BAD_REQUEST,
			     "XML syntax error")

	case Left(e) => throw e
      }
    } else {
      response.sendError(HttpServletResponse.SC_FORBIDDEN,
			 "try an absolute HTTP URI")
    }
  }
}
