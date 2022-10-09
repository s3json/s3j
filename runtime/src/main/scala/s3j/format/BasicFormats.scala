package s3j.format

import s3j.ast.{JsArray, JsObject, JsValue}
import s3j.format.impl.{JsValueFormat, NumberFormats, StringFormat}
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

  /** String format with unlimited length */
  given stringFormat: JsonFormat[String] = new StringFormat(Int.MaxValue)

  /** Standard UUID-as-string format */
  given uuidFormat: JsonFormat[UUID] with {
    def encode(writer: JsonWriter, value: UUID): Unit =
      writer.stringValue(value.toString)

    def decode(reader: JsonReader): UUID =
      UUID.fromString(DecoderUtils.decodeString(reader, 36, "too long string for UUID: length is 36 characters"))

    override def toString: String = "UuidFormat"
  }

  /** Format for parsing arbitrary JSON data as [[JsValue]] */
  given jsValueFormat: JsonFormat[JsValue] = JsValueFormat

  /** Format for parsing arbitrary JSON data, restricted to objects */
  given jsObjectFormat: JsonFormat[JsObject] with {
    def encode(writer: JsonWriter, value: JsObject): Unit = JsValueFormat.encode(writer, value)
    def decode(reader: JsonReader): JsObject = reader match {
      case reader: JsonReader.Buffered => reader.readValue() match {
        case obj: JsObject => obj
        case other => reader.parseError("expected object, got " + other.typeName)
      }

      case _ => reader.peekToken match {
        case JsonToken.TObjectStart => JsValueFormat.decodeStream(reader).asInstanceOf[JsObject]
        case other => DecoderUtils.throwUnexpected(reader, "object", other)
      }
    }
  }

  /** Format for parsing arbitrary JSON data, restricted to arrays */
  given jsArrayFormat: JsonFormat[JsArray] with {
    def encode(writer: JsonWriter, value: JsArray): Unit = JsValueFormat.encode(writer, value)
    def decode(reader: JsonReader): JsArray = reader match {
      case reader: JsonReader.Buffered => reader.readValue() match {
        case arr: JsArray => arr
        case other => reader.parseError("expected array, got " + other.typeName)
      }

      case _ => reader.peekToken match {
        case JsonToken.TArrayStart => JsValueFormat.decodeStream(reader).asInstanceOf[JsArray]
        case other => DecoderUtils.throwUnexpected(reader, "array", other)
      }
    }
  }
}
