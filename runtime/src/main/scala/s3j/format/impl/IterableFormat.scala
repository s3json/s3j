package s3j.format.impl

import s3j.format.util.ArrayFormatUtils
import s3j.format.{JsonDecoder, JsonEncoder, JsonFormat}
import s3j.io.{JsonReader, JsonToken, JsonWriter}

import scala.collection.Factory

// Single class for both encoding and decoding, inaccessible part is filled with nulls
private[format] class IterableFormat[T, C <: Iterable[T]](enc: JsonEncoder[T], dec: JsonDecoder[T], 
                                                          factory: Factory[T, C])
extends JsonFormat[C] {
  def encode(writer: JsonWriter, value: C): Unit = {
    val inner = ArrayFormatUtils.writeBeginArray(writer)
    
    for (v <- value) {
      enc.encode(inner, v)
    }
    
    ArrayFormatUtils.writeEndArray(writer)
  }

  def decode(reader: JsonReader): C = {
    val bld = factory.newBuilder
    val inner = ArrayFormatUtils.expectBeginArray(reader)
    
    while (inner.peekToken != JsonToken.TStructureEnd) {
      bld += dec.decode(inner)
    }
    
    ArrayFormatUtils.expectEndArray(reader)
    bld.result()
  }
}
