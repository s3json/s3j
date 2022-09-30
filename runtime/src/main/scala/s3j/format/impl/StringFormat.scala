package s3j.format.impl

import s3j.format.JsonFormat
import s3j.format.util.DecoderUtils
import s3j.io.{JsonReader, JsonToken, JsonWriter}

import scala.collection.mutable

class StringFormat(maxLength: Int) extends JsonFormat[String] {
  private def tooLongError(reader: JsonReader): Nothing =
    reader.parseError("string is too long: max length is " + maxLength)

  def decode(reader: JsonReader): String = reader.nextToken() match {
    case JsonToken.TString =>
      if (reader.chunk.remaining > maxLength) tooLongError(reader)
      reader.chunk.toString

    case JsonToken.TStringContinued =>
      val sb = new mutable.StringBuilder()
      var length = reader.chunk.remaining
      reader.chunk.appendTo(sb)

      var token = JsonToken.TStringContinued
      while (token == JsonToken.TStringContinued) {
        token = reader.nextToken()
        length += reader.chunk.remaining

        if (length > maxLength) tooLongError(reader)
        reader.chunk.appendTo(sb)
      }

      sb.result()

    case t => DecoderUtils.throwUnexpected(reader, "string", t)
  }

  def encode(writer: JsonWriter, value: String): Unit = writer.value(value)
}
