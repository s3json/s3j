package s3j.format.impl

import s3j.format.JsonFormat
import s3j.format.util.DecoderUtils
import s3j.io.{JsonReader, JsonToken, JsonWriter}

import scala.collection.mutable

class StringFormat(maxLength: Int) extends JsonFormat[String] {
  private def tooLongError(reader: JsonReader): Nothing =
    reader.parseError("string is too long: max length is " + maxLength)

  def decode(reader: JsonReader): String = {
    val r = DecoderUtils.decodeStringRaw(reader, maxLength)
    if (r == null) tooLongError(reader)
    else r
  }

  def encode(writer: JsonWriter, value: String): Unit = writer.stringValue(value)

  override def toString: String = s"StringFormat(maxLength = $maxLength)"
}
