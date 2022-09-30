package s3j.format.util

import s3j.format.{JsonDecoder, JsonEncoder, JsonFormat}
import s3j.io.{JsonReader, JsonWriter}

/**
 * Wrapper for recursive formats. Don't rely on this type too much, it will be removed at any time (if I will manage
 * to correctly generate wrapper class and @experimental's will be removed)
 */
object Recursive {
  class Decoder[T] extends JsonDecoder[T] {
    private var _underlying: JsonDecoder[T] = _
    def set(d: JsonDecoder[T]): Unit = _underlying = d
    def decode(reader: JsonReader): T = _underlying.decode(reader)
  }

  class Encoder[T] extends JsonEncoder[T] {
    private var _underlying: JsonEncoder[T] = _
    def set(d: JsonEncoder[T]): Unit = _underlying = d
    def encode(writer: JsonWriter, value: T): Unit = _underlying.encode(writer, value)
  }

  class Format[T] extends JsonFormat[T] {
    private var _underlying: JsonFormat[T] = _
    def set(d: JsonFormat[T]): Unit = _underlying = d
    def decode(reader: JsonReader): T = _underlying.decode(reader)
    def encode(writer: JsonWriter, value: T): Unit = _underlying.encode(writer, value)
  }
}
