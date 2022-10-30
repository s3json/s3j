package s3j

import sbt.Keys._
import sbt._

object S3jPlugin extends AutoPlugin {
  object autoImport extends S3jKeys {
    val s3j: S3jConstants.type = S3jConstants
  }
  
  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      libraryDependencies ++= Seq(
        S3jBuildInfo.organization %% "s3j-runtime"  % S3jBuildInfo.version,
        S3jBuildInfo.organization %% "s3j"          % S3jBuildInfo.version % Provided
      ),

      libraryDependencies ++= s3jModules.value
        .flatMap { mod =>
          mod.runtime.map(r => S3jBuildInfo.organization %% r % S3jBuildInfo.version) ++
            mod.provided.map(r => S3jBuildInfo.organization %% r % S3jBuildInfo.version % Provided)
        },

      s3jPlugins := Nil,
      s3jModules := Nil,
      s3jModifiers := Nil
    ) ++ inConfig(Compile)(configSettings) ++ inConfig(Test)(configSettings)

  private def configSettings: Seq[Def.Setting[_]] = Seq(
    scalacOptions ++= s3jPlugins.value.map(p => "-Xmacro-settings:s3j:usePlugin=" + p),
    scalacOptions ++= s3jModifiers.value.map(m => "-Xmacro-settings:s3j:@" + m)
  )
}
