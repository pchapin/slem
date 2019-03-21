import sbt._

// See: https://www.scala-sbt.org/1.x/docs/Organizing-Build.html
object Dependencies {

  // Versions
  lazy val scalaTestVersion = "3.0.5"
  lazy val kiamaVersion = "2.2.0"
  lazy val parserVersion = "1.1.1"

  // Libraries
  val scalactic = "org.scalactic" %% "scalactic" % scalaTestVersion
  val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion
  val kiama = "org.bitbucket.inkytonik.kiama" %% "kiama" % kiamaVersion
  val parser = "org.scala-lang.modules" %% "scala-parser-combinators" % parserVersion

  // Projects
  val slemDeps = Seq(scalactic, kiama, parser, scalaTest % Test)
}
