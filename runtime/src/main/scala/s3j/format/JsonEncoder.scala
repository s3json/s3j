package s3j.format

import s3j.io.JsonWriter

// Empty object is required so we could extend it later
object JsonEncoder

trait JsonEncoder[T] { outer =>
  /** Encode value into supplied JsonWriter */
  def encode(writer: JsonWriter, value: T): Unit

  /** @return [[JsonEncoder]] which applies a function to the encoded value */
  def mapEncoded[R](f: R => T): JsonEncoder[R] =
    new JsonEncoder[R] {
      def encode(writer: JsonWriter, value: R): Unit = outer.encode(writer, f(value))
      override def toString: String = s"$outer.mapEncoded($f)"
    }
}
