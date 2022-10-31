package s3j.schema.model

object CompositionSchema {
  val empty: CompositionSchema = CompositionSchema()
}

case class CompositionSchema(
  allOf:        Option[Seq[SchemaOrFalse]] = None,
  anyOf:        Option[Seq[SchemaOrFalse]] = None,
  oneOf:        Option[Seq[SchemaOrFalse]] = None,
  not:          Option[SchemaOrFalse] = None
)
