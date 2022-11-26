package s3j.io

import s3j.ast.JsValue
import s3j.format.impl.NumberFormats
import s3j.format.util.DecoderUtils
import s3j.io.util.CharRange

object JsonReader {
  /** Reader that already has AST available to avoid serialization and parsing round-trip */
  trait Buffered { this: JsonReader =>
    /**
     * {{{
     *  {"x": | { ... } }
     *  ^     ^ ^_ next value
     *  |     \_ cursor position
     *  \_ enclosing value
     * }}}
     * 
     * @return Enclosing value (i.e. value one level higher than current) 
     */  
    def readEnclosingValue(): JsValue
    
    /** Get enclosing value without consuming it */
    def enclosingValue: JsValue
    
    /** @return Next value (i.e. value that will be output via token stream) */
    def readValue(): JsValue

    /** Keys in current enclosing value which are not yet consumed, in stream order */
    def remainingKeys: Seq[String]
  }
}

/** High-level JSON reader, outputting a token stream */
abstract class JsonReader {
  private var _savedToken: Int = -1

  /** @return Next token from the stream */
  def nextToken(): JsonToken =
    if (_savedToken == -1) readToken()
    else {
      val r = _savedToken
      _savedToken = -1
      r
    }

  /** @return Peek token from the stream (token remains in the stream) */
  def peekToken: JsonToken = {
    if (_savedToken == -1) {
      _savedToken = readToken()
    }

    _savedToken
  }

  /** 
   * @return Next token from the stream. Implementations are assumed to implement own state machine and don't return
   *         illegal token sequences.
   */
  protected def readToken(): JsonToken

  /**
   * Implementations must return same object instance on all calls to this method, so returned instance could be cached
   * externally.
   *
   * @return Buffer holding a data associated with current chunk (string or number fragment)
   */
  def chunk: CharRange

  /** @return Handle for last processed key */
  def key: KeyHandle

  /** @return Location of last emitted token */
  def location: JsonLocation

  /** Throw parse error at current parser position */
  def parseError(msg: String): Nothing = throw ParseException(location, msg)
  
  /** Throw parse error at current parser position */
  def parseError(msg: String, cause: Throwable): Nothing = throw ParseException(location, msg, Some(cause))

  // Helpers to read primitives:
  // May be overridden to do it better than in generic case

  /** Decode boolean value from stream */
  def readBoolean(): Boolean =
    nextToken() match {
      case JsonToken.TTrueValue => true
      case JsonToken.TFalseValue => false
      case other => DecoderUtils.throwUnexpected(this, "boolean", other)
    }

  /** Decode byte value from stream */
  def readByte(): Byte = NumberFormats.decodeByte(this)

  /** Decode unsigned byte value from stream */
  def readUnsignedByte(): Byte = NumberFormats.decodeUnsignedByte(this)

  /** Decode short value from stream */
  def readShort(): Short = NumberFormats.decodeShort(this)

  /** Decode unsigned short value from stream */
  def readUnsignedShort(): Short = NumberFormats.decodeUnsignedShort(this)

  /** Decode int value from stream */
  def readInt(): Int = NumberFormats.decodeInt(this)

  /** Decode unsigned int value from stream */
  def readUnsignedInt(): Int = NumberFormats.decodeUnsignedInt(this)

  /** Decode long value from stream */
  def readLong(): Long = NumberFormats.decodeLong(this, false)

  /** Decode unsigned long value from stream */
  def readUnsignedLong(): Long = NumberFormats.decodeLong(this, true)

  /** Decode float value from stream */
  def readFloat(): Float = NumberFormats.decodeDouble(this).toFloat

  /** Decode double value from stream */
  def readDouble(): Double = NumberFormats.decodeDouble(this)

  /** Decode string value from the stream */
  def readString(): String = DecoderUtils.decodeStringRaw(this, Int.MaxValue)

  /**
   * Decode string value from stream up to length limit, returning `null` (and leaving reader in an undefined state)
   * when limit is exhausted
   */
  def readString(lengthLimit: Int): String | Null = DecoderUtils.decodeStringRaw(this, lengthLimit)
}
