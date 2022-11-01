package s3j.schema.model

import s3j.annotations.inlineObject
import s3j.ast.JsObject
import s3j.format.CollectionFormats
import s3j.{*, given}

object SchemaDocument {
  given schemaMapFormat: JsonFormat[Map[String, SchemaDocument]] = CollectionFormats.mapFormat
  given schemaSeqFormat: JsonFormat[Seq[SchemaDocument]] = CollectionFormats.iterableFormat

  /** JSON key with references */
  val ReferenceKey: String = "$ref"
}

/**
 * Parsed schema document model
 *
 * @param `$schema`   Schema URI
 * @param `$id`       Schema identifier
 * @param `$defs`     Sub-schema definitions
 * @param `$ref`      Reference to schema definition
 * @param annotations Annotations for the type (i.e. human-readable description)
 * @param s3j         s3j-specific annotations, describing more precise details of object
 * @param `type`      Basic type of the schema
 * @param format      Additional format information. JSON Schema specifies this field as a string-only, but de-facto it
 *                    is used for number too to specify precision.
 * @param string      String validations (meaningful only when `type == "string"`)
 * @param number      Number validations (meaningful only when `type == "integer" or "number"`)
 * @param `object`    Object validations (meaningful only when `type == "object"`)
 * @param array       Array validations (meaningful only when `type == "array"`)
 * @param restFields  Fields that aren't modelled by this class
 */
case class SchemaDocument(
  `$schema`:    Option[String] = None,
  `$id`:        Option[String] = None,
  `$defs`:      Option[Map[String, SchemaDocument]] = None,
  `$ref`:       Option[String] = None,

  @inlineObject annotations:  SchemaAnnotations = SchemaAnnotations.empty,

  `type`:       Option[SchemaType] = None,
  format:       Option[SchemaFormat] = None,
  nullable:     Option[Boolean] = None,
  s3j:          Option[S3jAnnotations] = None,

  @inlineObject string:       StringSchema = StringSchema.empty,
  @inlineObject number:       NumberSchema = NumberSchema.empty,
  @inlineObject `object`:     ObjectSchema = ObjectSchema.empty,
  @inlineObject array:        ArraySchema = ArraySchema.empty,
  @inlineObject composition:  CompositionSchema = CompositionSchema.empty,

  restFields:   JsObject = JsObject()
) derives JsonFormat {
  override def toString: String = this.toJsonString
}
