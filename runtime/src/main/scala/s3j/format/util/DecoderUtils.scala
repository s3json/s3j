package s3j.format.util

import s3j.format.JsonDecoder
import s3j.io.{JsonReader, JsonToken}

import scala.collection.mutable

object DecoderUtils {
  def throwUnexpected(reader: JsonReader, expected: String, token: JsonToken): Nothing =
    reader.parseError("unexpected " + JsonToken.tokenName(token) + ", expected " + expected)

  // null is decoded as None
  def decodeOption[T](reader: JsonReader, decoder: JsonDecoder[T]): Option[T] =
    if (reader.peekToken == JsonToken.TNullValue) { reader.nextToken(); None }
    else Some(decoder.decode(reader))

  def skipValue(r: JsonReader): Unit = {
    var nesting = 0
    var first = false

    while (first || nesting > 0) {
      first = false
      r.nextToken() match {
        case JsonToken.TEndOfStream => r.parseError("End of stream while parsing value")
        case JsonToken.TArrayStart => nesting += 1
        case JsonToken.TObjectStart => nesting += 1
        case JsonToken.TStructureEnd => nesting -= 1
        case _ => /* skip */
      }
    }
  }

  def decodeStringRaw(reader: JsonReader, maxLength: Int): String | Null = reader.nextToken() match {
    case JsonToken.TString =>
      if (reader.chunk.remaining > maxLength) null
      else reader.chunk.toString

    case JsonToken.TStringContinued =>
      val sb = new mutable.StringBuilder()
      var length = reader.chunk.remaining
      reader.chunk.appendTo(sb)

      var token = JsonToken.TStringContinued
      while (token == JsonToken.TStringContinued) {
        token = reader.nextToken()
        length += reader.chunk.remaining

        if (length > maxLength) return null
        reader.chunk.appendTo(sb)
      }

      sb.result()

    case t => DecoderUtils.throwUnexpected(reader, "string", t)
  }

  def decodeString(reader: JsonReader, maxLength: Int, limitMessage: String): String = {
    val r = decodeStringRaw(reader, maxLength)
    if (r == null) reader.parseError(limitMessage)
    else r
  }
}
