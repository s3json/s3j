package s3j

object S3jConstants {
  val version: String = S3jBuildInfo.version

  val akka: S3jModule = S3jModule(runtime = Set("s3j-akka"), provided = Set())
  val jooq: S3jModule = S3jModule(runtime = Set("s3j-jooq"), provided = Set())
  val schema: S3jModule = S3jModule(runtime = Set("s3j-schema"), provided = Set())
}
