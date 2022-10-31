package s3j.format.util

import s3j.format.{JsonDecoder, JsonEncoder}
import s3j.io.{JsonReader, JsonToken, JsonWriter}

import scala.collection.Factory

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

  def writeArray[T](writer: JsonWriter, arr: Iterable[T])(using enc: JsonEncoder[_ >: T]): Unit = {
    val inner = ArrayFormatUtils.writeBeginArray(writer)
    for (v <- arr) enc.encode(inner, v)
    ArrayFormatUtils.writeEndArray(writer)
  }

  def readArray[T, C <: Iterable[T]](reader: JsonReader)(using dec: JsonDecoder[T], factory: Factory[T, C]): C = {
    val bld = factory.newBuilder
    val inner = ArrayFormatUtils.expectBeginArray(reader)

    while (inner.peekToken != JsonToken.TStructureEnd) {
      bld += dec.decode(inner)
    }

    ArrayFormatUtils.expectEndArray(reader)
    bld.result()
  }
}
