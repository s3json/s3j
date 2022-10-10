package s3j.format
import s3j.format.util.DecoderUtils
import s3j.io.{JsonReader, JsonWriter}

import scala.util.control.NonFatal

trait StringyFormat[T] extends StringyEncoder[T] with StringyDecoder[T] { outer =>
  /** JSON format for encoding and decoding strings */
  override lazy val json: JsonFormat[T] =
    new JsonFormat[T] {
      def encode(writer: JsonWriter, value: T): Unit = writer.stringValue(outer.encode(value))

      // noinspection DuplicatedCode
      def decode(reader: JsonReader): T =
        try outer.decode(DecoderUtils.decodeString(reader, maxLength, "string is too long"))
        catch { case NonFatal(e) => reader.parseError(e.getMessage, e) }

      override def toString: String = s"$outer.json"
    }

  /** @return [[StringyFormat]] which applies functions on decoding and encoding */
  def mapFormat[R](encoding: R => T, decoding: T => R): StringyFormat[R] =
    new StringyFormat[R] {
      def encode(value: R): String = outer.encode(encoding(value))
      def decode(str: String): R = decoding(outer.decode(str))
      override def toString: String = s"$outer.mapFormat($encoding, $decoding)"
    }
}
