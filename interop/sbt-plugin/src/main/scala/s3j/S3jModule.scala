package s3j

import sbt.librarymanagement.ModuleID

object S3jModule {
  case object modules {
    val akka: S3jModule = S3jModule(runtime = Set("s3j-akka"), provided = Set())
    val jooq: S3jModule = S3jModule(runtime = Set("s3j-jooq"), provided = Set())
  }
}

case class S3jModule(runtime: Set[String], provided: Set[String])
