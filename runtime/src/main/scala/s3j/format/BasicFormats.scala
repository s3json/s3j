package s3j.format

import s3j.ast.JsValue
import s3j.format.impl.{JsValueFormat, StringFormat, NumberFormats}
import s3j.format.util.DecoderUtils
import s3j.io.{JsonReader, JsonToken, JsonWriter}

import java.util.UUID

object BasicFormats {
  given booleanFormat: JsonFormat[Boolean] with {
    def encode(writer: JsonWriter, value: Boolean): Unit = writer.boolValue(value)
    def decode(reader: JsonReader): Boolean = reader.nextToken() match {
      case JsonToken.TFalseValue => false
      case JsonToken.TTrueValue => true
      case other => DecoderUtils.throwUnexpected(reader, "boolean value", other)
    }

    override def toString: String = "booleanFormat"
  }

  export NumberFormats.given

  given stringFormat: JsonFormat[String] = new StringFormat(Int.MaxValue)

  given uuidFormat: JsonFormat[UUID] with {
    def encode(writer: JsonWriter, value: UUID): Unit =
      writer.stringValue(value.toString)

    def decode(reader: JsonReader): UUID =
      UUID.fromString(DecoderUtils.decodeString(reader, 36, "too long string for UUID: length is 36 characters"))

    override def toString: String = "UuidFormat"
  }

  given jsValueFormat: JsonFormat[JsValue] = new JsValueFormat
  // TODO: JsObject
  // TODO: JsArray


}
