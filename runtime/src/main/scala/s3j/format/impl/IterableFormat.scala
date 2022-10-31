package s3j.format.impl

import s3j.format.util.ArrayFormatUtils
import s3j.format.{JsonDecoder, JsonEncoder, JsonFormat}
import s3j.io.{JsonReader, JsonToken, JsonWriter}

import scala.collection.Factory

// Single class for both encoding and decoding, inaccessible part is filled with nulls
private[format] class IterableFormat[T, C <: Iterable[T]](enc: JsonEncoder[T], dec: JsonDecoder[T],
                                                          factory: Factory[T, C])
extends JsonFormat[C] {
  def encode(writer: JsonWriter, value: C): Unit =
    ArrayFormatUtils.writeArray(writer, value)(using enc)

  def decode(reader: JsonReader): C =
    ArrayFormatUtils.readArray[T, C](reader)(using dec, factory)

  override def toString: String = {
    if (enc == dec) s"IterableFormat($enc)"
    else if (enc == null) s"IterableDecoder($dec)"
    else if (dec == null) s"IterableEncoder($enc)"
    else s"IterableFormat(enc=$enc, dec=$dec)"
  }
}
