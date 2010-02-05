package org.w3.swap.webapp

/*
 * Front-side-of-a-page servlet docs seem kinda hard to find.
 * This seemed about my speed:
 * 
 * Step, a scala web picoframework
 * from gabriele renzi April 12th, 2009
 * http://www.riffraff.info/2009/4/11/step-a-scala-web-picoframework
 */

import javax.servlet.http.{HttpServlet,
			   HttpServletRequest, HttpServletResponse}

class HelloWorld extends HttpServlet {
  override def doGet(request: HttpServletRequest,
		     response: HttpServletResponse) {
    response.setContentType("text/html")
    response.getWriter.println("" + <p>it's <em>alive!</em></p>)
  }
}
