package s3j.schema.model

object ArraySchema {
  val empty: ArraySchema = ArraySchema()
}

case class ArraySchema(
  prefixItems:      Option[Seq[SchemaDocument]] = None,
  items:            Option[SchemaOrFalse] = None,
  unevaluatedItems: Option[SchemaOrFalse] = None,
  contains:         Option[SchemaDocument] = None,
  minContains:      Option[Int] = None,
  maxContains:      Option[Int] = None,
  minItems:         Option[Int] = None,
  maxItems:         Option[Int] = None,
  uniqueItems:      Option[Boolean] = None
)
