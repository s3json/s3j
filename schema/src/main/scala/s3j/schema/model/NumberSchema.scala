package s3j.schema.model

object NumberSchema {
  val empty: NumberSchema = NumberSchema()
}

case class NumberSchema(
  multipleOf:       Option[BigDecimal] = None,
  minimum:          Option[BigDecimal] = None,
  exclusiveMinimum: Option[BigDecimal] = None,
  maximum:          Option[BigDecimal] = None,
  exclusiveMaximum: Option[BigDecimal] = None
)
