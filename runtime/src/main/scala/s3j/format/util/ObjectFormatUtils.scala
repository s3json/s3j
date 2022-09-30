package s3j.format.util

import s3j.ast.{JsObject, JsValue}
import s3j.format.BasicFormats
import s3j.io.{JsonReader, JsonToken, JsonWriter}

object ObjectFormatUtils {
  def writeBeginObject(writer: JsonWriter): JsonWriter = {
    writer.beginObject()
    writer
  }

  def writeEndObject(writer: JsonWriter): Unit = {
    writer.end()
  }

  def throwMissingKey(reader: JsonReader, key: String): Nothing =
    reader.parseError(s"Missing required key '$key'")

  def throwUnknownKey(reader: JsonReader, key: String): Nothing =
    reader.parseError(s"Unknown key: '$key'")

  def expectBeginObject(reader: JsonReader): JsonReader = {
    val t = reader.nextToken()
    if (t != JsonToken.TObjectStart) {
      reader.parseError("Expected start of object, got " + JsonToken.tokenName(t) + " instead")
    }

    reader
  }

  def expectEndObject(reader: JsonReader): Unit = {
    val t = reader.nextToken()
    if (t != JsonToken.TStructureEnd) {
      reader.parseError("Expected end of object, got " + JsonToken.tokenName(t) + " instead")
    }
  }

  def writeRestFields(writer: JsonWriter, value: JsObject): Unit =
    for (k <- value.keysIterator) {
      writer.key(k)
      BasicFormats.jsValueFormat.encode(writer, value(k))
    }

  class RestFieldsBuilder {
    private val items = Map.newBuilder[String, JsValue]
    private val order = Vector.newBuilder[String]

    def readField(key: String, reader: JsonReader): Unit = {
      order += key
      items += key -> BasicFormats.jsValueFormat.decode(reader)
    }

    def result(): JsObject = new JsObject(items.result(), order.result())
  }
}
