package s3j.format
import s3j.io.{JsonReader, JsonWriter, ParseException}

import scala.util.control.NonFatal

object JsonFormat {
  /** Combine [[JsonEncoder]] and [[JsonDecoder]] into single [[JsonFormat]] */
  def of[T](encoder: JsonEncoder[_ >: T], decoder: JsonDecoder[_ <: T]): JsonFormat[T] =
    new JsonFormat[T] {
      def encode(writer: JsonWriter, value: T): Unit = encoder.encode(writer, value)
      def decode(reader: JsonReader): T = decoder.decode(reader)
      override def toString: String = s"JsonFormat.of($encoder, $decoder)"
    }
}

trait JsonFormat[T] extends JsonEncoder[T] with JsonDecoder[T] { outer =>
  /** @return [[JsonFormat]] which applies the functions to encoding and decoding */
  def mapFormat[R](encoding: R => T, decoding: T => R): JsonFormat[R] =
    new JsonFormat[R] {
      def encode(writer: JsonWriter, value: R): Unit = outer.encode(writer, encoding(value))
      def decode(reader: JsonReader): R =
        try decoding(outer.decode(reader))
        catch {
          case e: ParseException => throw e
          case NonFatal(e) => reader.parseError(e.toString, e)
        }

      override def toString: String = s"$outer.mapFormat($encoding, $decoding)"
    }
}
