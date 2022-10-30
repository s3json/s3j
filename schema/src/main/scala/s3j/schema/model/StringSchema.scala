package s3j.schema.model

object StringSchema {
  val empty: StringSchema = StringSchema()
}

case class StringSchema(
  minLength:        Option[Int] = None,
  maxLength:        Option[Int] = None,
  pattern:          Option[String] = None,
  contentMediaType: Option[String] = None,
  contentEncoding:  Option[StringEncodingSchema] = None
)
