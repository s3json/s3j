package s3j.io

import s3j.io.util.{EscapeUtils, WriterStateMachine}

import java.io.Writer

object StreamJsonWriter {
  private val IndentSpaceBuffer = Array.fill[Char](32)(' ')

  val DefaultSettings: WriterSettings = WriterSettings()

  /**
   * Settings that control how JSON output is rendered.
   *
   * @param indent         Indentation used for pretty-printing. A value of zero produces compact output; a non-zero
   *                       value specifies the number of spaces per nesting level.
   * @param escapeHtml     Whether to escape HTML-sensitive characters that could be unsafe when the output is embedded
   *                       in a `<script>` tag.
   * @param escapeUnicode  Whether to escape non-printable Unicode characters that are valid in JSON but may cause
   *                       rendering issues, ambiguity, or difficulties when handled manually.
   * @param escapeEightBit Whether to escape any character outside the basic ASCII range.
   */
  case class WriterSettings(
    indent:         Int = 0,
    escapeHtml:     Boolean = true,
    escapeUnicode:  Boolean = true,
    escapeEightBit: Boolean = false,
  )
}

/**
 * Writer implementation backed by a character stream.
 *
 * @param out      Underlying writer to use
 * @param settings Settings that control JSON output formatting and escaping.
 */
class StreamJsonWriter(
  out: Writer,
  settings: StreamJsonWriter.WriterSettings = StreamJsonWriter.DefaultSettings
) extends JsonWriter {
  private class StackEntry(val isRoot: Boolean = false, val isArray: Boolean = false, var isString: Boolean = false) {
    var hasValues: Boolean = false
    var firstChunk: Boolean = true
  }

  private var currentIndent = 0
  private var states: List[StackEntry] = new StackEntry(isRoot = true) :: Nil
  private val stateMachine: WriterStateMachine = new WriterStateMachine
  private def state: StackEntry = states.head

  private val indent = settings.indent

  private val escapeClasses: Byte = {
    var result = EscapeUtils.CEscapeAlways
    if (settings.escapeHtml) result |= EscapeUtils.CEscapeHtml
    if (settings.escapeUnicode) result |= EscapeUtils.CEscapeDiscretionary
    if (settings.escapeEightBit) result |= EscapeUtils.CEscape8bit
    result.toByte
  }

  private def writeNewline(): Unit = {
    if (indent == 0) {
      return
    }

    var remaining = currentIndent
    out.write('\n')

    while (remaining > 0) {
      val toWrite = remaining min StreamJsonWriter.IndentSpaceBuffer.length
      out.write(StreamJsonWriter.IndentSpaceBuffer, 0, toWrite)
      remaining -= toWrite
    }
  }

  private def postValue(): Unit = {
    if (state.hasValues) {
      out.write(',')
    }

    state.hasValues = true
    state.firstChunk = true
  }

  private def preStructStart(): Unit = {
    preValue()
  }

  private def preStructEnd(): Unit = {
    if (state.hasValues) {
      writeNewline()
    }
  }

  private def preKey(): Unit = {
    postValue()
    writeNewline()
  }

  private def preValue(): Unit = {
    if (state.isArray) {
      postValue()
      writeNewline()
    }
  }

  private def writeString(str: String): Unit = {
    out.write('"')
    EscapeUtils.writeEscaped(str, escapeClasses, out)
    out.write('"')
  }

  def beginArray(): JsonWriter = {
    stateMachine.beginArray()
    preStructStart()
    out.write('[')
    currentIndent += indent
    states = new StackEntry(isArray = true) :: states
    this
  }

  def beginObject(): JsonWriter = {
    stateMachine.beginObject()
    preStructStart()
    out.write('{')
    currentIndent += indent
    states = new StackEntry(isArray = false) :: states
    this
  }

  def beginString(): JsonWriter = {
    stateMachine.beginString()
    preValue()
    out.write('"')
    states = new StackEntry(isString = true) :: states
    this
  }

  def end(): JsonWriter = {
    stateMachine.end()
    currentIndent -= indent
    preStructEnd()
    out.write(if (states.head.isArray) ']' else if (states.head.isString) '"' else '}')
    states = states.tail
    this
  }

  def key(key: String): JsonWriter = {
    stateMachine.key()
    preKey()
    writeString(key)
    out.write(if (indent != 0) ": " else ":")
    this
  }

  def boolValue(value: Boolean): JsonWriter = {
    stateMachine.value()
    preValue()
    out.write(if (value) "true" else "false")
    this
  }

  def longValue(value: Long): JsonWriter = {
    stateMachine.value()
    preValue()
    out.write(java.lang.Long.toString(value, 10))
    this
  }
  
  def unsignedLongValue(value: Long): JsonWriter = {
    stateMachine.value()
    preValue()
    out.write(java.lang.Long.toUnsignedString(value, 10))
    this
  }

  def doubleValue(value: Double): JsonWriter = {
    stateMachine.value()
    preValue()
    if (value.isFinite) out.write(value.toString)
    else if (value.isNaN) out.write("\"NaN\"")
    else if (value.isPosInfinity) out.write("\"Infinity\"")
    else out.write("\"-Infinity\"")
    this
  }

  def bigintValue(value: BigInt): JsonWriter = {
    stateMachine.value()
    preValue()
    out.write(value.toString)
    this
  }

  def bigdecValue(value: BigDecimal): JsonWriter = {
    stateMachine.value()
    preValue()
    out.write(value.toString)
    this
  }

  def stringValue(value: String): JsonWriter = {
    if (state.isString) {
      EscapeUtils.writeEscaped(value, escapeClasses, out)
      return this
    }

    stateMachine.value()
    preValue()
    writeString(value)
    this
  }

  def stringValue(value: Array[Char], offset: Int, length: Int): JsonWriter = {
    if (state.isString) {
      EscapeUtils.writeEscaped(value, offset, length, escapeClasses, out)
      return this
    }

    stateMachine.value()
    preValue()
    out.write('"')
    EscapeUtils.writeEscaped(value, offset, length, escapeClasses, out)
    out.write('"')
    this
  }

  def nullValue(): JsonWriter = {
    stateMachine.value()
    preValue()
    out.write("null")
    this
  }

  def haveRawChunks: Boolean = true

  def rawChunk(chunk: Array[Char], offset: Int, length: Int): JsonWriter = {
    if (state.isString) {
      EscapeUtils.writeEscaped(chunk, offset, length, escapeClasses, out)
    } else {
      if (state.firstChunk) {
        stateMachine.value()
        preValue()
        state.firstChunk = false
      }

      out.write(chunk, offset, length)
    }

    this
  }

  /** Finish writing (checking for correctness of the output) and close this writer */
  def finish(): Unit = {
    stateMachine.finish()
    close()
  }

  /**
    * Close this [[JsonWriter]], releasing all its resources.
    *
    * $checksState
    * $returnThis
    */
  def close(): Unit = {
    out.close()
  }
}
