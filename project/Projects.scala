import sbt._

class Projects {
  lazy val sourcecode = project.in(file("layerSourceCode"))
  lazy val bytecode = project.in(file("layerByteCode"))
}
