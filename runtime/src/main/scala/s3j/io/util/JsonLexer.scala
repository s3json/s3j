package s3j.io.util

import s3j.io.StreamPosition
import s3j.io.util.JsonLexer.*
import s3j.io.util.LexerConst.*

import java.io.Reader
import scala.annotation.switch

object JsonLexer {
  // Lexical tokens are more low-level than usual JSON tokens:
  final val LTEndOfStream = 0             // end of stream is reached
  final val LTBeginObject = 1             // start of object    '{'
  final val LTBeginArray = 2              // start of array     '['
  final val LTEndObject = 3               // end of object      '}'
  final val LTEndArray = 4                // end of array       ']'
  final val LTComma = 5                   // value separator    ','
  final val LTColon = 6                   // key-val separator  ':'
  final val LTTrueValue = 7               // 'true'   value
  final val LTFalseValue = 8              // 'false'  value
  final val LTNullValue = 9               // 'null'   value
  final val LTStringContinued = 10        // string chunk, will be continued in following chunks
  final val LTString = 11                 // string chunk, last
  final val LTNumberContinued = 12        // number chunk, will be continued in following chunks
  final val LTNumber = 13                 // number chunk, last

  def tokenName(t: Int): String = TokenNames(t)

  case class LexerException(message: String, position: StreamPosition) extends RuntimeException(message)
}

/**
 * Lexical analyzer for JSON character stream. Recognizes language entities such as strings, numbers and literals, but
 * does not keep any object stack and does not perform any grammar validation (e.g. can't distinguish keys from values).
 *
 * Keeps a limited amount of context for error reporting. Upon encountering an error, gives out a small window of
 * surrounding text to make debugging easier.
 *
 * For performance reasons all advanced state (other than token type) is presented via mutable internal structures, and
 * is valid only for last returned token. String data chunks are pre-processed, i.e. all escape sequences are replaced
 * by their meaning.
 *
 * @param in            Underlying reader to use
 * @param chunkLength   Maximum length of parsed and emitted chunks (e.g. for strings or numbers)
 * @param contextLength Length of contextual areas in buffer. There are two contextual areas (leading and trailing), so
 *                      actual emitted context string will be two times longer.
 */
class JsonLexer(in: Reader, chunkLength: Int, val contextLength: Int) {
  private val buffer = new CharRange(chunkLength + 2 * contextLength) // private parsing buffer
  val chunk = new CharRange(chunkLength) // public chunk with processed data
  private val unescape = new UnescapingContext
  private var endOfStream = false

  if (chunkLength < EscapeUtils.EscapeLength) {
    // It must be able to hold at least one full escape sequence
    throw new IllegalArgumentException("Too small parsing chunk: must be at least " + EscapeUtils.EscapeLength)
  }

  buffer.limit = 0

  private var state = SNormal
  private var streamOffset = 0 // absolute stream position of buffer start
  private var lineNumber = 1
  private var lineOffset = 0
  private var startPosition = 0

  private inline def remaining: Int = if (endOfStream) buffer.remaining else buffer.remaining - contextLength
  private inline def hasRemaining: Boolean = hasRemaining(1)
  private inline def hasRemaining(n: Int): Boolean = remaining >= n

  /** scan to-be-discarded part of buffer to advance any position counters */
  private def advancePosition(length: Int): Unit = {
    val buf: Array[Char] = buffer.data
    var pos: Int = 0

    while (pos < length) {
      if (buf(pos) == '\n') {
        lineNumber += 1
        lineOffset = streamOffset + pos + 1
      }

      pos += 1
    }

    streamOffset += length
  }

  /** @return Position of last read token */
  def position: StreamPosition = position(startPosition)

  /**
   * Materialize position object for specified point in current buffer
   *
   * @param bufferPosition  Absolute index of character in current buffer
   * @return                Materialized position object
   */
  private def position(bufferPosition: Int): StreamPosition = {
    // These counters are valid at the beginning of buffer...
    var lineNumber = this.lineNumber
    var lineOffset = this.lineOffset

    // ...so we should actualize them to desired point
    var idx = 0
    while (idx < bufferPosition) {
      if (buffer.data(idx) == '\n') {
        lineNumber = lineNumber + 1
        lineOffset = streamOffset + idx + 1
      }

      idx += 1
    }

    val contextStart = math.max(0, bufferPosition - contextLength)
    val contextEnd = math.min(buffer.limit, bufferPosition + contextLength)

    StreamPosition(
      offset = streamOffset + bufferPosition,
      line = lineNumber,
      column = streamOffset - lineOffset + bufferPosition,
      context = new String(buffer.data, contextStart, contextEnd - contextStart),
      contextOffset = bufferPosition - contextStart
    )
  }

  /** Discard already processed parts and refill the buffer from the reader */
  private def refillBuffer(): Unit = {
    val prefixLength = math.min(contextLength, buffer.position)

    val compacted = buffer.position - prefixLength
    advancePosition(compacted)
    startPosition -= compacted

    buffer.compactPrefix(prefixLength)

    var headroom = buffer.capacity - buffer.limit
    while (!endOfStream && headroom > 0) {
      val read = in.read(buffer.data, buffer.limit, headroom)
      if (read == -1) endOfStream = true
      else {
        buffer.limit += read
        headroom -= read
      }
    }
  }

  /** Start or continue reading of string literal. 'chunk' buffer gets unescaped data */
  private def readString(): Int = {
    if (!hasRemaining(unescape.partial + 1)) {
      if (endOfStream) return LTEndOfStream
      else refillBuffer()
    }

    unescape.reset()
    chunk.clear()

    while (!unescape.hitQuote && chunk.hasRemaining && hasRemaining(unescape.partial + 1)) {
      try unescape.process(buffer, chunk, streamMode = true)
      catch {
        case e: InvalidEscapeException =>
          throw LexerException(e.message, position(buffer.position + e.position))
      }
    }

    chunk.flip()

    if (unescape.hitQuote) {
      state = SNormal
      buffer.get() // consume that quote
      LTString
    } else if (chunk.limit == 0 && endOfStream) LTEndOfStream
    else LTStringContinued
  }

  /**
   * Start or continue reading of number. No further validations are performed, any number-like string is considered
   * a number (e.g. `12..3e123e+-123` is a number too). It's up to upstream number parser to actually validate it.
   */
  private def readNumber(): Int = {
    chunk.clear()

    if (!endOfStream && !hasRemaining) {
      refillBuffer()
    }

    var finished: Boolean = endOfStream && !hasRemaining
    while (!finished && chunk.hasRemaining && hasRemaining) {
      var cls: Byte = 0
      val buf: Array[Char] = buffer.data
      var pos: Int = buffer.position
      val end: Int = math.min(buffer.limit, pos + chunk.remaining)

      while (pos < end && {
        cls = charClass(buf(pos))
        classType(cls) == CtStateSwitch && cvsIsNumber(classValue(cls))
      }) pos += 1

      chunk.put(buf, buffer.position, pos - buffer.position)
      buffer.position = pos

      finished = pos < end || (endOfStream && !hasRemaining)
    }

    chunk.flip()

    if (finished) {
      state = SNormal
      LTNumber
    } else LTNumberContinued
  }

  /** Read and consume specified literal from input stream. Index is `Cvl*` constant from [[LexerConst]] object */
  private def readLiteral(literalIdx: Int): Int = {
    val data = LiteralData(literalIdx)
    var ofs = 0

    while (ofs < data.length) {
      if (!hasRemaining) {
        if (!endOfStream) refillBuffer()
        else throw LexerException("end of stream while reading '" + LiteralText(literalIdx) + "' literal", position)
      }

      val chr = buffer.get()
      if (data(ofs) != chr) {
        throw LexerException("unexpected '" + chr + "' while reading '" + LiteralText(literalIdx) +
          "' literal", position)
      }

      ofs += 1
    }

    LiteralTokens(literalIdx)
  }

  /** Read next lexical token from stream. Returned values are from `LT*` constant set of [[JsonLexer]] object. */
  def readToken(): Int = {
    startPosition = buffer.position

    state match {
      case SNormal =>
        while (true) {
          startPosition = buffer.position

          if (!hasRemaining) {
            refillBuffer()
            if (endOfStream && !hasRemaining) return LTEndOfStream
          }

          val chr = buffer.get()
          val chrClass = charClass(chr)

          (classType(chrClass): @switch) match {
            case CtDirect => return classValue(chrClass)
            case CtInvalid => throw LexerException("invalid JSON character '" + chr + "'", position)
            case CtWhitespace => // skip
            case CtStateSwitch => classValue(chrClass) match {
              case CvsNumberStart =>
                buffer.adjustPosition(-1) // get that char back
                state = SNumber
                return readNumber()

              case CvsString =>
                state = SString
                return readString()

              case CvsNumberPart =>
                throw LexerException("invalid JSON character: number cannot start with '" + chr + "'", position)
            }

            case CtLiteral => return readLiteral(classValue(chrClass))
          }
        }

        // Unreachable
        throw new RuntimeException()

      case SString => readString()
      case SNumber => readNumber()
    }
  }
}
