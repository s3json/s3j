package s3j.io.util

import java.io.{StringWriter, Writer}

object EscapeUtils {
  // Character.isXXX lookups is rather costly, so cache everything in a LUT
  private val _shouldEscape: Array[Byte] = generateShouldEscape()
  private val _hexAlphabet: Array[Char] = "0123456789ABCDEF".toCharArray

  /** Maximum possible escape length */
  val EscapeLength: Int = 6

  /** @return Whether character 'c' should be escaped or could be used as-is */
  def shouldEscape(c: Char): Boolean = _shouldEscape(c) != 0

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
  def writeEscaped(data: Array[Char], offset: Int, length: Int, out: Writer): Unit = {
    var idx: Int = offset
    val end: Int = offset + length
    var esc: Array[Char] | Null = null

    while (idx < end) {
      var nextEscaped: Int = idx
      while (nextEscaped < end && _shouldEscape(data(nextEscaped)) == 0) nextEscaped += 1

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
  def writeEscaped(str: String, out: Writer): Unit = {
    var idx: Int = 0
    val end: Int = str.length
    var esc: Array[Char] | Null = null

    while (idx < end) {
      var nextEscaped: Int = idx
      while (nextEscaped < end && _shouldEscape(str.charAt(nextEscaped)) == 0) nextEscaped += 1

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
  def escape(s: String): String = {
    val sw = new StringWriter()
    writeEscaped(s, sw)
    sw.toString
  }

  private def generateShouldEscape(): Array[Byte] = {
    val r = new Array[Byte](65536)

    for (c <- '\u0000' to '\uFFFF') {
      // We always escape '<' to be on safe side if output is embedded on the web page.
      // E.g. valid JSON in following example triggers XSS because browser parses HTML first, and then JSON:
      //
      //  <script>window.state = {"test":"</script><script>alert(1)</script>"}</script>
      //
      // We escape '<' instead of usual '/' to get much more fancier URLs which are much more common than tags.

      val esc = c == '"' || c == '\\' || c == '<' || !((c >= ' ' && c < 127) || Character.isAlphabetic(c) &&
        Character.isLetterOrDigit(c))

      r(c) = if (esc) 1 else 0
    }

    r
  }
}
