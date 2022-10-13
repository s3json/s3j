ThisBuild / organization := "io.s3j"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.2.0"

val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.14" % Test
  ),

  Compile / scalacOptions ++= Seq("-Xcheck-macros"),
)

lazy val root = (project in file("."))
  .aggregate(runtime, core, `interop-akka`)
  .settings(
    name := "s3j-root",
    publishArtifact := false,
    publish := {},
    publishLocal := {}
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
    name := "s3j",

    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % Provided
    )
  )

lazy val `interop-akka` = (project in file("interop/akka"))
  .dependsOn(runtime)
  .settings(commonSettings)
  .settings(
    name := "s3j-akka",

    libraryDependencies ++= Seq(
      // Apache 2 licensed versions:
      "com.typesafe.akka" %% "akka-stream" % "2.6.20",
      "com.typesafe.akka" %% "akka-http" % "10.2.10"
    ).map(_.cross(CrossVersion.for3Use2_13))
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
