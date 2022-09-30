package s3j.io

import s3j.ast.JsValue
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
    
    /** @return Next value (i.e. value that will be output via token stream) */
    def readValue(): JsValue
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
}
