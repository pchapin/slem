import sbt._

// See: https://www.scala-sbt.org/1.x/docs/Organizing-Build.html
object Dependencies {

  // Versions
  lazy val scalaTestVersion = "3.0.5"
  lazy val kiamaVersion = "2.2.0"

  // Libraries
  val scalactic = "org.scalactic" %% "scalactic" % scalaTestVersion
  val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion
  val kiama = "org.bitbucket.inkytonik.kiama" %% "kiama" % kiamaVersion

  // Projects
  val slemDeps = Seq(scalactic, kiama, scalaTest % Test)
}
