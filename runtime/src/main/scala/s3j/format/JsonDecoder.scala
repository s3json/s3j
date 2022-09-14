package s3j.format

import s3j.io.JsonReader

// Empty object is required so we could extend it later
object JsonDecoder

trait JsonDecoder[T] {
  /** Consume supplied JsonReader and decode a value of type T */
  def decode(reader: JsonReader): T
}
