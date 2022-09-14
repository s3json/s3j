package s3j.format

import s3j.io.JsonWriter

// Empty object is required so we could extend it later
object JsonEncoder

trait JsonEncoder[T] {
  /** Encode value into supplied JsonWriter */
  def encode(writer: JsonWriter, value: T): Unit
}
