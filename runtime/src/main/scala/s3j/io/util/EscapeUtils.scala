package s3j.io.util

import java.io.{StringWriter, Writer}

object EscapeUtils {
  // Character.isXXX lookups is rather costly, so cache everything in a LUT
  private val _shouldEscape: Array[Byte] = generateShouldEscape()
  private val _hexAlphabet: Array[Char] = "0123456789ABCDEF".toCharArray

  // Escape classes in _shouldEscape:
  final val CEscapeAlways         = 1   // Mandatory per RFC8259
  final val CEscapeHtml           = 2   // Security-relevant when inserted into <script>
  final val CEscapeDiscretionary  = 4   // Weird Unicode characters that would be unpleasant to see directly
  final val CEscape8bit           = 8   // Any 8-bit character, leaving 7-bit output only.

  final val EscapeAll: Byte = 0xFF.toByte // all classes combined

  /** Maximum possible escape length */
  val EscapeLength: Int = 6

  /** @return Whether character 'c' should be escaped or could be used as-is */
  def shouldEscape(c: Char, mask: Byte): Boolean = (_shouldEscape(c) & mask) != 0

  /** Place escaped version of character `c` into array `out` and return length of the escape */
  def formatEscape(c: Char, out: Array[Char]): Int = {
    out(0) = '\\'

    c match {
      case '\"' => out(1) = '\"'; /* return */ 2
      case '\\' => out(1) = '\\'; /* return */ 2
      case '\b' => out(1) = 'b'; /* return */ 2
      case '\f' => out(1) = 'f'; /* return */ 2
      case '\n' => out(1) = 'n'; /* return */ 2
      case '\r' => out(1) = 'r'; /* return */ 2
      case '\t' => out(1) = 't'; /* return */ 2
      case '/' => out(1) = '/'; /* return */ 2 // not used by this library, but included for completeness
      case _ =>
        out(1) = 'u'
        out(2) = _hexAlphabet(c >> 12)
        out(3) = _hexAlphabet((c >> 8) & 15)
        out(4) = _hexAlphabet((c >> 4) & 15)
        out(5) = _hexAlphabet(c & 15)
        /* return */ 6
    }
  }

  /** Write escaped data from given char array into writer */
  def writeEscaped(data: Array[Char], offset: Int, length: Int, escapeClasses: Byte, out: Writer): Unit = {
    var idx: Int = offset
    val end: Int = offset + length
    var esc: Array[Char] | Null = null

    while (idx < end) {
      var nextEscaped: Int = idx
      while (nextEscaped < end && (_shouldEscape(data(nextEscaped)) & escapeClasses) == 0) nextEscaped += 1

      if (nextEscaped != end) {
        // noinspection DuplicatedCode
        if (nextEscaped != idx) {
          out.write(data, idx, nextEscaped - idx)
        }

        if (esc == null) {
          esc = new Array[Char](EscapeLength)
        }

        out.write(esc, 0, formatEscape(data(nextEscaped), esc))
        idx = nextEscaped + 1
      } else {
        out.write(data, idx, end - idx)
        idx = end
      }
    }
  }

  /** Write escaped version of given string into writer */
  def writeEscaped(str: String, escapeClasses: Byte, out: Writer): Unit = {
    var idx: Int = 0
    val end: Int = str.length
    var esc: Array[Char] | Null = null

    while (idx < end) {
      var nextEscaped: Int = idx
      while (nextEscaped < end && (_shouldEscape(str.charAt(nextEscaped)) & escapeClasses) == 0) nextEscaped += 1

      if (nextEscaped != end) {
        // noinspection DuplicatedCode
        if (nextEscaped != idx) {
          out.write(str, idx, nextEscaped - idx)
        }

        if (esc == null) {
          esc = new Array[Char](EscapeLength)
        }

        out.write(esc, 0, formatEscape(str.charAt(nextEscaped), esc))
        idx = nextEscaped + 1
      } else {
        out.write(str, idx, end - idx)
        idx = end
      }
    }
  }

  /** Get escaped version of a string */
  def escape(s: String, escapeClasses: Byte = EscapeAll): String = {
    val sw = new StringWriter()
    writeEscaped(s, escapeClasses, sw)
    sw.toString
  }

  private def generateShouldEscape(): Array[Byte] = {
    val r = new Array[Byte](65536)

    for (c <- '\u0000' to '\uFFFF') {
      var cls = 0

      if (c < ' ' || c == '"' || c == '\\') {
        cls |= CEscapeAlways
      }

      if (c == '<' || c == '>' || c == '&' || c == '\u2028' || c == '\u2029') {
        cls |= CEscapeHtml
      }

      if (c >= 127 && !(Character.isAlphabetic(c) || Character.isLetterOrDigit(c))) {
        cls |= CEscapeDiscretionary
      }

      if (c >= 127) {
        cls |= CEscape8bit
      }

      r(c) = cls.toByte
    }

    r
  }
}
