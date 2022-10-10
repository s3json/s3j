package s3j.format.impl

import s3j.format.util.ObjectFormatUtils
import s3j.format.{JsonDecoder, JsonEncoder, JsonFormat, StringyDecoder, StringyEncoder}
import s3j.io.{JsonReader, JsonToken, JsonWriter}

// Single class for both encoding and decoding, inaccessible part is filled with nulls
private[format] class MapFormat[K, V](kEnc: StringyEncoder[K], kDec: StringyDecoder[K], 
                                      vEnc: JsonEncoder[V], vDec: JsonDecoder[V])
extends JsonFormat[Map[K, V]] {
  def encode(writer: JsonWriter, value: Map[K, V]): Unit = {
    val inner = ObjectFormatUtils.writeBeginObject(writer)
    for ((k, v) <- value) {
      inner.key(kEnc.encode(k))
      vEnc.encode(inner, v)
    }
    ObjectFormatUtils.writeEndObject(writer)
  }

  def decode(reader: JsonReader): Map[K, V] = {
    val inner = ObjectFormatUtils.expectBeginObject(reader)
    val bld = Map.newBuilder[K, V]

    while (inner.peekToken == JsonToken.TKey) {
      inner.nextToken()
      val key = kDec.decode(inner.key.toString)
      val value = vDec.decode(inner)

      bld += key -> value
    }

    ObjectFormatUtils.expectEndObject(reader)
    bld.result()
  }
}
