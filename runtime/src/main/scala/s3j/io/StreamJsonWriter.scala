package s3j.io

import s3j.io.util.{EscapeUtils, WriterStateMachine}

import java.io.Writer

object StreamJsonWriter {
  private val IndentSpaceBuffer = Array.fill[Char](32)(' ')
}

/**
 * Writer implementation backed by a character stream.
 *
 * @param out    Underlying writer to use
 * @param indent Indentation for the output, `0` - compact format.
 */
class StreamJsonWriter(out: Writer, indent: Int = 0) extends JsonWriter {
  private class StackEntry(val isRoot: Boolean = false, val isArray: Boolean = false, var isString: Boolean = false) {
    var hasValues: Boolean = false
    var firstChunk: Boolean = true
  }

  private var currentIndent = 0
  private var states: List[StackEntry] = new StackEntry(isRoot = true) :: Nil
  private val stateMachine: WriterStateMachine = new WriterStateMachine
  private def state: StackEntry = states.head

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
    EscapeUtils.writeEscaped(str, out)
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

  def value(value: Boolean): JsonWriter = {
    stateMachine.value()
    preValue()
    out.write(if (value) "true" else "false")
    this
  }

  def value(value: Long): JsonWriter = {
    stateMachine.value()
    preValue()
    out.write(value.toString)
    this
  }

  def value(value: Double): JsonWriter = {
    stateMachine.value()
    preValue()
    out.write(value.toString)
    this
  }

  def value(value: BigInt): JsonWriter = {
    stateMachine.value()
    preValue()
    out.write(value.toString)
    this
  }

  def value(value: BigDecimal): JsonWriter = {
    stateMachine.value()
    preValue()
    out.write(value.toString)
    this
  }

  def value(value: String): JsonWriter = {
    if (state.isString) {
      EscapeUtils.writeEscaped(value, out)
      return this
    }

    stateMachine.value()
    preValue()
    writeString(value)
    this
  }

  def value(value: Array[Char], offset: Int, length: Int): JsonWriter = {
    if (state.isString) {
      EscapeUtils.writeEscaped(value, offset, length, out)
      return this
    }

    stateMachine.value()
    preValue()
    out.write('"')
    EscapeUtils.writeEscaped(value, offset, length, out)
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
      EscapeUtils.writeEscaped(chunk, offset, length, out)
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
