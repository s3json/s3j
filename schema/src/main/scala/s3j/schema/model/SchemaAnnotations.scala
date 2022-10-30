package s3j.schema.model

import s3j.ast.JsValue

object SchemaAnnotations {
  val empty: SchemaAnnotations = SchemaAnnotations()
}

case class SchemaAnnotations(
  title:        Option[String] = None,
  description:  Option[String] = None,
  default:      Option[JsValue] = None,
  examples:     Option[Seq[JsValue]] = None,
  deprecated:   Option[Boolean] = None,
  readOnly:     Option[Boolean] = None,
  writeOnly:    Option[Boolean] = None
)
