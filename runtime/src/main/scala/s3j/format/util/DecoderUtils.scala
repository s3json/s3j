package s3j.format.util

import s3j.format.JsonDecoder
import s3j.io.{JsonReader, JsonToken}

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
}
