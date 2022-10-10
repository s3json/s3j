package s3j.format
import s3j.format.util.DecoderUtils
import s3j.io.JsonReader

import scala.util.control.NonFatal

trait StringyDecoder[T] { outer =>
  /** @return Decoded instance of T */
  def decode(str: String): T

  /** @return Maximum length of possible encoding */
  def maxLength: Int = Int.MaxValue

  /** JSON encoder for encoding strings */
  lazy val json: JsonDecoder[T] =
    new JsonDecoder[T] {
      // noinspection DuplicatedCode
      def decode(reader: JsonReader): T =
        try outer.decode(DecoderUtils.decodeString(reader, maxLength, "string is too long"))
        catch { case NonFatal(e) => reader.parseError(e.getMessage, e) }

      override def toString: String = s"$outer.json"
    }

  /** @return [[JsonDecoder]] which applies a function to the decoded value */
  def mapDecoded[R](f: T => R): StringyDecoder[R] =
    new StringyDecoder[R] {
      def decode(str: String): R = f(outer.decode(str))
      override def toString: String = s"$outer.mapDecoded($f)"
    }
}
