package s3j.schema

import s3j.schema.model.SchemaDocument
import s3j.*
import s3j.ast.JsValue

object JsonSchema

/**
 * Raw JSON schema definition for type `T`.
 *
 * @param schemaJson    Raw JSON schema string. References are specified as `"$ref": "~NNN"`, where `NNN` is an index
 *                      in [[definitions]] array.
 * @param definitions   Definitions for references used in [[schemaJson]], each index in reference prefixed with `~`.
 * @param shouldInline  Whether this schema should be inlined when bundling, rather than referenced.
 * @param defaultFn     Getter for the default value, when its serialized version cannot be computed statically.
 * @param examplesFn    Getter for the example values, when their serialized versions cannot be computed statically.
 * @tparam T            Type described by this schema. Isn't actually used anywhere in the class.
 */
class JsonSchema[T](
  schemaJson:     String,
  definitions:    Seq[() => JsonSchema[_]] = Nil,
  shouldInline:   Boolean = false,
  defaultFn:      Option[() => JsValue] = None,
  examplesFn:     Option[() => Seq[JsValue]] = None
) {
  /** Parsed JSON schema document, with correct default and example values */
  lazy val document: SchemaDocument = {
    val basicDoc: SchemaDocument = schemaJson.fromJson[SchemaDocument]
    if (defaultFn.isEmpty && examplesFn.isEmpty) basicDoc
    else {
      val annotations = basicDoc.annotations
      basicDoc.copy(annotations = annotations.copy(
        default = defaultFn.map(_.apply()).orElse(annotations.default),
        examples = examplesFn.map(_.apply()).orElse(annotations.examples)
      ))
    }
  }

  /** Schema document as JSON value */
  def jsonValue: JsValue = document.toJsonValue

  /** Schema document as JSON string */
  def json: String = document.toJsonString
  
  /** Schema document as JSON pretty-printed string */
  def jsonPretty: String = document.toJsonString(2)

  override def toString: String = s"JsonSchema($json, nDefs=${definitions.size})"
}
