val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-staging" % scala3Version,
      "com.lihaoyi" %% "utest" % "0.7.10" % "test"
    ),

    testFrameworks += new TestFramework("utest.runner.Framework")
  )
