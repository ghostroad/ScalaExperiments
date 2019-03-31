val scalactic = "org.scalactic" %% "scalactic" % "3.0.5"
val scalatest = "org.scalatest" %% "scalatest" % "3.0.5" % "test"

lazy val commonSettings = Seq(
  organization := "com.mushfeq.euler",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.12.6"
)

lazy val root = (project in file("."))
  .settings(
    commonSettings,
    name := "Euler",
    libraryDependencies ++= Seq(scalactic, scalatest)
  )