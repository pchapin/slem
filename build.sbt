import Dependencies._

ThisBuild / organization := "org.slem"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.8"

logBuffered in Test := false

lazy val slem = (project in file("."))
  .settings(
    name := "SLEM",
    libraryDependencies ++= slemDeps,
  )
