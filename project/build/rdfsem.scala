/* sbt config to use scalatest
 *
 * references:
 * http://www.scalatest.org/
 * http://code.google.com/p/simple-build-tool/wiki/BuildConfiguration
 */
import sbt.{ProjectInfo, DefaultProject}

class ScalaTestProject(info: ProjectInfo) extends DefaultProject(info)
{
  val scalatest = "org.scalatest" % "scalatest" % "0.9.5" % "test->default"
}
