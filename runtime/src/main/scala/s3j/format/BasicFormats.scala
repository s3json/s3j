package s3j.format

import s3j.ast.{JsArray, JsObject, JsValue}
import s3j.format.impl.{JsValueFormat, NumberFormats, StringFormat}
import s3j.format.util.DecoderUtils
import s3j.io.{JsonReader, JsonToken, JsonWriter}

import java.util.UUID

trait LowPriorityBasicFormats {
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

object BasicFormats extends LowPriorityBasicFormats {
  given booleanFormat: JsonFormat[Boolean] with {
    def encode(writer: JsonWriter, value: Boolean): Unit = writer.boolValue(value)
    def decode(reader: JsonReader): Boolean = reader.nextToken() match {
      case JsonToken.TFalseValue => false
      case JsonToken.TTrueValue => true
      case other => DecoderUtils.throwUnexpected(reader, "boolean value", other)
    }

    override def toString: String = "booleanFormat"
  }

  /** Stringy boolean format */
  given stringyBooleanFormat: StringyFormat[Boolean] with {
    def encode(value: Boolean): String = value.toString
    def decode(str: String): Boolean = java.lang.Boolean.parseBoolean(str)
    override def toString: String = "stringyBooleanFormat"
  }

  export NumberFormats.given

  /** String format with unlimited length */
  given stringFormat: JsonFormat[String] = new StringFormat(Int.MaxValue)

  /** Stringy format for strings */
  given stringStringyFormat: StringyFormat[String] with {
    def encode(value: String): String = value
    def decode(str: String): String = str
    override def toString: String = "stringStringyFormat"
  }

  /** Standard UUID-as-string format */
  given uuidFormat: JsonFormat[UUID] with {
    def encode(writer: JsonWriter, value: UUID): Unit =
      writer.stringValue(value.toString)

    def decode(reader: JsonReader): UUID =
      UUID.fromString(DecoderUtils.decodeString(reader, 36, "too long string for UUID: length is 36 characters"))

    override def toString: String = "uuidFormat"
  }

  /** Stringy format for UUIDs */
  given uuidStringyFormat: StringyFormat[UUID] with {
    def encode(value: UUID): String = value.toString
    def decode(str: String): UUID = UUID.fromString(str)
    override def toString: String = "uuidStringyFormat"
  }

  /** Format for parsing arbitrary JSON data as [[JsValue]] */
  given jsValueFormat: JsonFormat[JsValue] = JsValueFormat
}
