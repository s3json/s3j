package s3j.format.impl

import s3j.ast.*
import s3j.format.JsonFormat
import s3j.io.{JsonReader, JsonWriter, JsonToken}

import scala.collection.mutable

object JsValueFormat extends JsonFormat[JsValue] {
  private def readString(reader: JsonReader, continuation: Int, lengthLimit: Int): String = {
    val sb = new mutable.StringBuilder()
    var length = reader.chunk.remaining
    reader.chunk.appendTo(sb)

    var token = continuation
    while (token == continuation) {
      if (length > lengthLimit) {
        reader.parseError("value is too long: limit is " + lengthLimit + " characters")
      }

      token = reader.nextToken()
      reader.chunk.appendTo(sb)
      length += reader.chunk.remaining
    }

    sb.result()
  }

  private def readNumber(s: String): JsNumber = JsBigDecimal(BigDecimal(s))

  def decodeStream(reader: JsonReader): JsValue = reader.nextToken() match {
    case JsonToken.TEndOfStream => reader.parseError("Unexpected end of stream")
    case JsonToken.TObjectStart =>
      val items = Map.newBuilder[String, JsValue]
      val order = Vector.newBuilder[String]
      while (reader.peekToken == JsonToken.TKey) {
        val key = reader.key.toString
        reader.nextToken() // consume key

        items += key -> decodeStream(reader)
        order += key
      }

      assert(reader.nextToken() == JsonToken.TStructureEnd)
      new JsObject(items.result(), order.result())

    case JsonToken.TArrayStart =>
      val items = Vector.newBuilder[JsValue]
      while (reader.peekToken != JsonToken.TStructureEnd) {
        items += decodeStream(reader)
      }

      assert(reader.nextToken() == JsonToken.TStructureEnd)
      new JsArray(items.result())

    case JsonToken.TTrueValue   => JsBoolean.JsTrue
    case JsonToken.TFalseValue  => JsBoolean.JsFalse
    case JsonToken.TNullValue   => JsNull

    case JsonToken.TString      => JsString(reader.chunk.toString)
    case JsonToken.TStringContinued => JsString(readString(reader, JsonToken.TStringContinued, Int.MaxValue))

    case JsonToken.TNumber => readNumber(reader.chunk.toString)
    case JsonToken.TNumberContinued => readNumber(readString(reader, JsonToken.TNumberContinued, 1024))

    case JsonToken.TKey => reader.parseError("encountered object key when trying to parse a value")
  }

  def decode(reader: JsonReader): JsValue = reader match {
    case buf: JsonReader.Buffered => buf.readValue()
    case _ => decodeStream(reader)
  }

  def encode(writer: JsonWriter, value: JsValue): Unit = value match {
    case JsNull           => writer.nullValue()
    case JsBoolean(value) => writer.boolValue(value)
    case JsString(value)  => writer.stringValue(value)

    case number: JsNumber => number match {
      case JsInt(value)         => writer.longValue(value)
      case JsLong(value)        => writer.longValue(value)
      case JsFloat(value)       => writer.doubleValue(value)
      case JsDouble(value)      => writer.doubleValue(value)
      case JsBigInt(value)      => writer.bigintValue(value)
      case JsBigDecimal(value)  => writer.bigdecValue(value)
    }

    case array: JsArray =>
      writer.beginArray()
      for (a <- array.value) encode(writer, a)
      writer.end()

    case obj: JsObject =>
      writer.beginObject()
      for (k <- obj.keysIterator) encode(writer.key(k), obj(k))
      writer.end()
  }
  
  override def toString: String = "JsValueFormat"
}
