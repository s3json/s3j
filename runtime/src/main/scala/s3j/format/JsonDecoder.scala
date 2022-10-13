package s3j.format

import s3j.io.{JsonReader, ParseException}

import scala.util.control.NonFatal

object JsonDecoder {
  /**
   * Generated codecs are materialized by macro engine without asking, meaning that every call site will get it's own
   * JSON codec instance. Useful mostly in situations where JSON codec is a part of a larger type class, which is then
   * derived explicitly by user. Otherwise using this type may lead to silent code bloat, generating huge classes over
   * and over again.
   */
  trait Generated[T] { given codec: JsonDecoder[T] }
}

trait JsonDecoder[T] { outer =>
  /** Consume supplied JsonReader and decode a value of type T */
  def decode(reader: JsonReader): T

  /** @return [[JsonDecoder]] which applies a function to the decoded value */
  def mapDecoded[R](f: T => R): JsonDecoder[R] =
    new JsonDecoder[R] {
      def decode(reader: JsonReader): R =
        try f(outer.decode(reader))
        catch {
          case e: ParseException => throw e
          case NonFatal(e) => reader.parseError(e.getMessage, e)
        }

      override def toString: String = s"$outer.mapDecoded($f)"
    }
}
