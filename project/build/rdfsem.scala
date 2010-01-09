/* sbt config to use scalatest
 *
 * references:
 * http://www.scalatest.org/
 * http://code.google.com/p/simple-build-tool/wiki/BuildConfiguration
 */
import sbt.{ProjectInfo, DefaultProject}

class TestingProject(info: ProjectInfo) extends DefaultProject(info)
{
  val scalatest = "org.scalatest" % "scalatest" % "0.9.5" % "test->default"

  // per http://code.google.com/p/scalacheck/
// 1.6 isn't in the maven repo :-/
//  val scalacheck = "org.scalacheck" % "scalacheck" % "1.6" % "test->default"
//  val toolsrep = "Scala-Tools Repo" at "http://scala-tools.org"
}
