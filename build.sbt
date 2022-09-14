ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .aggregate(runtime, core)
  .settings(
    name := "s3j-root",
    publishArtifact := false,
    publishTo := None
  )

lazy val runtime = (project in file("runtime"))
  .settings(
    name := "s3j-runtime",

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.12" % Test
    )
  )

lazy val core = (project in file("core"))
  .dependsOn(runtime)
  .settings(
    name := "s3j"
  )