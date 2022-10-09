package s3j.format.util

import s3j.io.util.CharRange
import s3j.io.{JsonLocation, JsonReader, JsonToken, KeyHandle}

/**
 * JSON reader that adds top level object to token stream, allowing object decoder to be called to continue reading of
 * in-flight object.
 *
 * Recognized by [[ObjectFormatUtils.expectBeginObject]] and [[ObjectFormatUtils.expectEndObject]]: it will incur no
 * overhead at all for anything using these functions to decode objects.
 */
final case class AddObjectReader(reader: JsonReader) extends JsonReader {
  private var _objectStarted: Boolean = false
  private var _nesting: Int = 0
  
  protected def readToken(): JsonToken =
    if (_objectStarted) {
      reader.peekToken match {
        case JsonToken.TStructureEnd if _nesting == 0 => JsonToken.TStructureEnd // do not consume!
        case JsonToken.TStructureEnd =>
          _nesting -= 1
          reader.nextToken()

        case JsonToken.TObjectStart | JsonToken.TArrayStart =>
          _nesting += 1
          reader.nextToken()

        case _ => reader.nextToken()
      }
    } else {
      _objectStarted = true
      JsonToken.TObjectStart
    }
  
  def chunk: CharRange = reader.chunk
  def key: KeyHandle = reader.key
  def location: JsonLocation = reader.location
}
