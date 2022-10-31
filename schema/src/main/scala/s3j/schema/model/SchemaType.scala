package s3j.schema.model

import s3j.format.util.{ArrayFormatUtils, DecoderUtils}
import s3j.io.{JsonReader, JsonToken, JsonWriter}
import s3j.{*, given}

object SchemaType {
  private val innerFormat: JsonFormat[SchemaType] = JsonFormat.derived
  given schemaTypeWrapper: Conversion[SchemaType, Seq[SchemaType]] = _ :: Nil

  given schemaTypeFormat: JsonFormat[Seq[SchemaType]] with {
    def encode(writer: JsonWriter, value: Seq[SchemaType]): Unit =
      if (value.size == 1) innerFormat.encode(writer, value.head)
      else ArrayFormatUtils.writeArray(writer, value)(using innerFormat)

    def decode(reader: JsonReader): Seq[SchemaType] =
      reader.peekToken match {
        case JsonToken.TArrayStart => ArrayFormatUtils.readArray[SchemaType, Seq[SchemaType]](reader)(using innerFormat)
        case JsonToken.TString | JsonToken.TStringContinued => innerFormat.decode(reader)
        case other => DecoderUtils.throwUnexpected(reader, "string or array of strings", other)
      }
  }
}

enum SchemaType {
  /** String schema type, specific keywords for this type are in [[StringSchema]] */
  case String

  /** Number schema type (floating), specific keywords for this type are in [[NumberSchema]] */
  case Number

  /** Integer schema type, specific keywords for this type are in [[NumberSchema]] */
  case Integer

  /** Object schema type, specific keywords for this type are in [[ObjectSchema]] */
  case Object
  
  /** Array schema type, specific keywords for this type are in [[ArraySchema]] */
  case Array
  
  /** Boolean schema type (no specific keywords for this type) */
  case Boolean
  
  /** Null schema type (no specific keywords for this type) */
  case Null

  /** Custom format not modelled by this enumeration */
  case Custom(value: String)
}
