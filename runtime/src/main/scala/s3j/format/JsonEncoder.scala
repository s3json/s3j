package s3j.format

import s3j.ast.JsValue
import s3j.io.{JsonWriter, AstJsonWriter}

object JsonEncoder {
  /**
   * Generated codecs are materialized by macro engine without asking, meaning that every call site will get it's own
   * JSON codec instance. Useful mostly in situations where JSON codec is a part of a larger type class, which is then
   * derived explicitly by user. Otherwise using this type may lead to silent code bloat, generating huge classes over 
   * and over again.
   */
  trait Generated[T] { given codec: JsonEncoder[T] }
}

trait JsonEncoder[T] { outer =>
  /** Encode value into supplied JsonWriter */
  def encode(writer: JsonWriter, value: T): Unit

  /** Encode value into JsValue */
  def encodeValue(value: T): JsValue = {
    val writer = new AstJsonWriter
    encode(writer, value)
    writer.result()
  }

  /** @return [[JsonEncoder]] which applies a function to the encoded value */
  def mapEncoded[R](f: R => T): JsonEncoder[R] =
    new JsonEncoder[R] {
      def encode(writer: JsonWriter, value: R): Unit = outer.encode(writer, f(value))
      override def toString: String = s"$outer.mapEncoded($f)"
    }
}
