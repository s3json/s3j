package s3j.format.util

import s3j.io.{JsonReader, JsonToken, JsonWriter}

object ArrayFormatUtils {
  def writeBeginArray(writer: JsonWriter): JsonWriter = 
    writer.beginArray()
    
  def writeEndArray(writer: JsonWriter): Unit =
    writer.end()
    
  def expectBeginArray(reader: JsonReader): JsonReader =
    reader.nextToken() match {
      case JsonToken.TArrayStart => reader
      case other => DecoderUtils.throwUnexpected(reader, "array", other)
    }
    
  def expectEndArray(reader: JsonReader): Unit =
    reader.nextToken() match {
      case JsonToken.TStructureEnd => // ok
      case other => DecoderUtils.throwUnexpected(reader, "end of array", other)
    }
}
