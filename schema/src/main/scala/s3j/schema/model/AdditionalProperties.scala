package s3j.schema.model

import s3j.format.util.DecoderUtils
import s3j.format.JsonFormat
import s3j.io.{JsonReader, JsonToken, JsonWriter}

object AdditionalProperties {
  given schemaWrapper: Conversion[SchemaDocument, AdditionalProperties] = AdditionalProperties.Schema(_)
  
  given additionalPropsFormat: JsonFormat[AdditionalProperties] with {
    private def document: JsonFormat[SchemaDocument] = implicitly

    def encode(writer: JsonWriter, value: AdditionalProperties): Unit =
      value match {
        case AdditionalProperties.Forbidden => writer.boolValue(false)
        case AdditionalProperties.Schema(schema) => document.encode(writer, schema)
      }

    def decode(reader: JsonReader): AdditionalProperties =
      reader.peekToken match {
        case JsonToken.TFalseValue =>
          reader.nextToken()
          AdditionalProperties.Forbidden

        case JsonToken.TObjectStart =>
          AdditionalProperties.Schema(document.decode(reader))

        case other => DecoderUtils.throwUnexpected(reader, "false or object", other)
      }
  }
}

enum AdditionalProperties {
  /** Additional properties are forbidden (serialized as `false`) */
  case Forbidden

  /** Additional properties matching given schema are allowed */
  case Schema(schema: SchemaDocument)
}
