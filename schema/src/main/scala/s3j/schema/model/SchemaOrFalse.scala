package s3j.schema.model

import s3j.format.util.DecoderUtils
import s3j.format.JsonFormat
import s3j.io.{JsonReader, JsonToken, JsonWriter}

object SchemaOrFalse {
  given schemaWrapper: Conversion[SchemaDocument, SchemaOrFalse] = SchemaOrFalse.Schema(_)
  
  given additionalPropsFormat: JsonFormat[SchemaOrFalse] with {
    private def document: JsonFormat[SchemaDocument] = implicitly

    def encode(writer: JsonWriter, value: SchemaOrFalse): Unit =
      value match {
        case SchemaOrFalse.Forbidden => writer.boolValue(false)
        case SchemaOrFalse.Schema(schema) => document.encode(writer, schema)
      }

    def decode(reader: JsonReader): SchemaOrFalse =
      reader.peekToken match {
        case JsonToken.TFalseValue =>
          reader.nextToken()
          SchemaOrFalse.Forbidden

        case JsonToken.TObjectStart =>
          SchemaOrFalse.Schema(document.decode(reader))

        case other => DecoderUtils.throwUnexpected(reader, "false or object", other)
      }
  }
}

enum SchemaOrFalse {
  /** Additional properties are forbidden (serialized as `false`) */
  case Forbidden

  /** Additional properties matching given schema are allowed */
  case Schema(schema: SchemaDocument)
}
