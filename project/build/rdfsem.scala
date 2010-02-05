/* sbt config to use scalatest
 *
 * references:
 * http://www.scalatest.org/
 * http://code.google.com/p/simple-build-tool/wiki/BuildConfiguration
 */
import sbt.{ProjectInfo, DefaultProject, DefaultWebProject}

class TestingProject(info: ProjectInfo) extends DefaultWebProject(info)
{
  /* per SethTisue in #scala */
  override def compileOptions =
    "-g -unchecked -encoding us-ascii"
     .split(" ").map(CompileOption).toSeq ++ super.compileOptions

  /* workaround for:
   * scalac Parameter '-linksource' is not recognised by Scalac.
   *
   * ack: mharrah in #scala on Freenode
   * */
  override def documentOptions = Nil

  /*
   * val scalatest = "org.scalatest" % "scalatest" % "1.0" % "test->default"
   */
  val scalatest = "org.scalatest" % "scalatest" % "1.0.1-for-scala-2.8.0.Beta1-RC5-with-test-interfaces-0.2-SNAPSHOT" % "test"

  /* per http://code.google.com/p/scalacheck/
   * err... maybe
   * http://groups.google.com/group/scalacheck/browse_thread/thread/09f5d84f40fe405a
   */


  /*
   * <SethTisue> for ScalaCheck, I'm using val scalacheck = "org.scala-tools.testing" % "scalacheck_2.8.0.Beta1-RC5" % "1.7-SNAPSHOT" % "test"
   */

  val scalacheck = "org.scala-tools.testing" % "scalacheck_2.8.0.Beta1-RC5" % "1.7-SNAPSHOT" % "test"
  val toolsrep = "Scala-Tools Repo" at "http://www.scala-tools.org/repo-releases"
  val bleedingedge = "Scala-Tools testing" at "http://www.scala-tools.org/repo-snapshots/"


  // per http://code.google.com/p/simple-build-tool/wiki/WebApplications
  // and http://code.google.com/p/simple-build-tool/wiki/WebApplicationExample

  val jetty6 = "org.mortbay.jetty" % "jetty" % "6.1.14" % "test"
  val servlet = "javax.servlet" % "servlet-api" % "2.5" % "provided"
}
