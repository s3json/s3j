package s3j.format

import s3j.io.{JsonReader, ParseException}

import scala.util.control.NonFatal

// Empty object is required so we could extend it later
object JsonDecoder

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
