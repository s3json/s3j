package s3j

import sbt._

trait S3jKeys {
  val s3jPlugins    = settingKey[Seq[String]]("s3j global plugins to load")
  val s3jModifiers  = settingKey[Seq[String]]("s3j global modifiers to use")
  val s3jModules    = settingKey[Seq[S3jModule]]("s3j interop modules")
}
