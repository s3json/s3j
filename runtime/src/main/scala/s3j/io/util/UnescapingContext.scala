package s3j.io.util

import scala.annotation.switch

/** Mutable object to hold unescaping state. Could be reused to process input incrementally. */
final class UnescapingContext {
  private var _hitQuote: Boolean = false
  private var _partial: Int = 0
  private var _consumed: Int = 0

  private def decodeUnicode(src: Array[Char], srcIdx: Int, position: Int): Char = {
    var r = 0
    var i = 0

    while (i < 4) {
      val chr = src(srcIdx + i)
      var digit = 0

      if (chr >= '0' && chr <= '9') digit = chr - '0'
      else if (chr >= 'a' && chr <= 'f') digit = chr - 'a' + 10
      else if (chr >= 'A' && chr <= 'F') digit = chr - 'A' + 10
      else {
        val sb = new java.lang.StringBuilder
        sb.append("Invalid hexadecimal digit '").append(chr).append("' in unicode escape sequence: ")
        sb.append(src, srcIdx - 2, 6)
        throw new InvalidEscapeException(position, 6, sb.toString)
      }

      r = (r << 4) | digit
      i += 1
    }

    r.toChar
  }

  private inline def throwRange(): Nothing = throw new RuntimeException("range error")

  /** Reset state to default values */
  def reset(): Unit = {
    _hitQuote = false
    _consumed = 0
    _partial = 0
  }

  /**
   * Decode escape sequences in buffer [[src]] and write output to buffer [[dst]]. On return, source position is
   * advanced by the number of successfully consumed characters, destination position is set to the number of produced
   * characters (which may be less than number of consumed). Any incomplete escape sequence that occurs immediately
   * before end of source buffer is not consumed, but it's presence is reported via result of [[partial]] method.
   *
   * When [[streamMode]] is false, parser will ignore any closing quotes and treat them as plain text. It will also
   *    ignore characters prohibited by JSON syntax (such as newlines).
   *
   * When [[streamMode]] is true, parser will stop on first closing quote. This condition is signalled by result of
   *   [[hitQuote]] method. Quote is not consumed. Parser will fail on prohibited characters.
   *
   * Any [[UnescapeException]]s thrown out of this method will have it's position set relative to initial source buffer
   * position. Buffer positions and limits are not changed when this method returns exceptionally.
   *
   * @param src Source buffer
   * @param dst Destination buffer
   * @param streamMode Whether parsing should stop at the first quote
   */
  def process(src: CharRange, dst: CharRange, streamMode: Boolean): Unit = {
    _hitQuote = false
    _partial = 0

    val srcBuf = src.data
    var srcIdx = src.position
    val srcStart = srcIdx
    val srcEnd = src.limit

    val dstBuf = dst.data
    var dstIdx = dst.position
    val dstEnd = dst.limit

    var continue = true
    while (continue) {
      val searchEnd = srcIdx + math.min(srcEnd - srcIdx, dstEnd - dstIdx)
      var nextEscape = srcIdx

      if (nextEscape < 0 || searchEnd > srcBuf.length) throwRange()

      if (streamMode) {
        var c: Char = 0
        while (nextEscape < searchEnd && { c = srcBuf(nextEscape); c >= ' ' && c != '\\' && c != '"' }) nextEscape += 1

        if (nextEscape < searchEnd && c < ' ') {
          throw new InvalidEscapeException(srcIdx - src.position, 1,
            "Invalid character in JSON string: \\u%04x".format(c & 0xFFFF))
        }
      } else {
        while (nextEscape < searchEnd && srcBuf(nextEscape) != '\\') nextEscape += 1
      }

      val toCopy = nextEscape - srcIdx
      System.arraycopy(srcBuf, srcIdx, dstBuf, dstIdx, toCopy)
      srcIdx += toCopy
      dstIdx += toCopy

      if (nextEscape == searchEnd) continue = false
      else if (streamMode && srcBuf(nextEscape) == '"') {
        continue = false
        _hitQuote = true
      } else if (nextEscape == srcEnd - 1) {
        // we got just '\' and then buffer ended
        continue = false
        _partial = 1
      } else {
        var r: Char = 0
        var escapeLength = 2

        (srcBuf(srcIdx + 1): @switch) match {
          case '\\' => r = '\\'
          case '"'  => r = '"'
          case '/'  => r = '/'
          case 'n'  => r = '\n'
          case 'r'  => r = '\r'
          case 't'  => r = '\t'
          case 'b'  => r = '\b'
          case 'f'  => r = '\f'
          case 'u'  =>
            if (srcEnd < srcIdx + 6) {
              _partial = srcEnd - srcIdx
              continue = false
            } else {
              r = decodeUnicode(srcBuf, srcIdx + 2, srcIdx - src.position)
              escapeLength = 6
            }

          case chr =>
            throw new InvalidEscapeException(srcIdx - src.position, 2, "Unrecognized escape sequence: \\" + chr)
        }

        if (continue) {
          dstBuf(dstIdx) = r
          srcIdx += escapeLength
          dstIdx += 1
        }
      }
    }

    _consumed = srcIdx - srcStart
    src.position = srcIdx
    dst.position = dstIdx
  }

  /** @return Number of consumed characters in last iteration */
  def consumed: Int = _consumed

  /** @return Number of characters left in source buffer that constitute partial escape sequence */
  def partial: Int = _partial

  /** @return Whether last invocation of [[process]] was stopped by hitting a closing quote */
  def hitQuote: Boolean = _hitQuote
}
