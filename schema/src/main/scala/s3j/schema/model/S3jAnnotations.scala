package s3j.schema.model

import s3j.{*, given}
import s3j.ast.JsObject

object S3jAnnotations {
  val empty: S3jAnnotations = S3jAnnotations()
}

/**
 * S3j-specific JSON schema annotations
 */
case class S3jAnnotations(
  restFields:   JsObject = JsObject()
) derives JsonFormat
