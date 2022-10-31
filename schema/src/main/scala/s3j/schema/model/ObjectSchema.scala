package s3j.schema.model

import s3j.*
import s3j.format.util.DecoderUtils
import s3j.io.{JsonReader, JsonToken, JsonWriter}

object ObjectSchema {
  val empty: ObjectSchema = ObjectSchema()
}

case class ObjectSchema(
  properties:             Option[OrderedMap[String, SchemaDocument]] = None,
  patternProperties:      Option[OrderedMap[String, SchemaDocument]] = None,
  additionalProperties:   Option[SchemaOrFalse] = None,
  unevaluatedProperties:  Option[SchemaOrFalse] = None,
  required:               Option[Set[String]] = None,
  propertyNames:          Option[SchemaDocument] = None,
  minProperties:          Option[Int] = None,
  maxProperties:          Option[Int] = None
)
