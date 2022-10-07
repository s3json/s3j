ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.2.0"

val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.12" % Test
  ),

  Compile / scalacOptions ++= Seq("-Xcheck-macros"),
)

lazy val root = (project in file("."))
  .aggregate(runtime, core)
  .settings(
    name := "s3j-root",
    publishArtifact := false,
    publishTo := None
  )

lazy val runtime = (project in file("runtime"))
  .settings(commonSettings)
  .settings(
    name := "s3j-runtime"
  )

lazy val core = (project in file("core"))
  .dependsOn(runtime)
  .settings(commonSettings)
  .settings(
    name := "s3j"
  )

def exampleProject(exampleName: String): Project = Project("example-" + exampleName, file("examples/" + exampleName))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "s3j-example-" + exampleName,
    publishArtifact := false,
    publishTo := None
  )

lazy val exampleCustomPlugin = exampleProject("custom-plugin")
