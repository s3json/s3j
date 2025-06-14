ThisBuild / organization := "io.s3j"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.6"

val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.15" % Test
  ),

  Compile / scalacOptions ++= Seq("-Xcheck-macros", "-Yno-decode-stacktraces"),
)

val notPublished = Seq(
  publishArtifact := false,
  publishTo := None,
  publish := {},
  publishLocal := {}
)

lazy val root = (project in file("."))
  .aggregate(runtime, schema, core, `interop-akka`, `interop-jooq`, `interop-sbt`)
  .settings(notPublished)
  .settings(
    name := "s3j-root"
  )

lazy val runtime = (project in file("runtime"))
  .settings(commonSettings)
  .settings(
    name := "s3j-runtime"
  )

lazy val `core-internal` = (project in file("core-internal"))
  .dependsOn(runtime)
  .settings(commonSettings, notPublished)
  .settings(
    name := "s3j-core-internal",
  )

lazy val schema = (project in file("schema"))
  .dependsOn(runtime, `core-internal` % "compile-internal, test-internal")
  .settings(commonSettings)
  .settings(
    name := "s3j-schema"
  )

lazy val core = (project in file("core"))
  .dependsOn(runtime, schema)
  .settings(commonSettings)
  .settings(
    name := "s3j",

    libraryDependencies ++= Seq(
      "io.s3j" %% "s3j-macro-helpers" % "0.2.1",
    )
  )

// noinspection SbtDependencyVersionInspection
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

lazy val `interop-jooq` = (project in file("interop/jooq"))
  .dependsOn(runtime, core % Test)
  .settings(commonSettings)
  .settings(
    name := "s3j-jooq",

    libraryDependencies ++= Seq(
      "org.jooq"            % "jooq"        % "3.18.0",
      "org.testcontainers"  % "postgresql"  % "1.19.2" % Test,
      "org.postgresql"      % "postgresql"  % "42.5.4" % Test
    )
  )

lazy val `interop-sbt` = (project in file("interop/sbt-plugin"))
  .enablePlugins(BuildInfoPlugin, SbtPlugin)
  .settings(
    name := "s3j-sbt",

    scalaVersion := "2.12.20",
    crossScalaVersions := Seq(scalaVersion.value),

    buildInfoKeys := Seq(organization, version),
    buildInfoPackage := "s3j",
    buildInfoObject := "S3jBuildInfo",
    buildInfoOptions += BuildInfoOption.PackagePrivate
  )

def exampleProject(exampleName: String): Project = Project("example-" + exampleName, file("examples/" + exampleName))
  .dependsOn(core)
  .settings(commonSettings, notPublished)
  .settings(name := "s3j-example-" + exampleName)

lazy val exampleCustomPlugin = exampleProject("custom-plugin").dependsOn(schema)
