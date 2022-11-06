package s3j.io

import s3j.ast.{JsArray, JsBigDecimal, JsBigInt, JsBoolean, JsDouble, JsLong, JsNull, JsObject, JsString, JsValue}
import s3j.io.util.WriterStateMachine

import java.nio.{ByteBuffer, ByteOrder}
import scala.collection.mutable

object AstJsonWriter {
  private final val TObject = 0
  private final val TArray  = 1
  private final val TString = 2
  private final val TRoot   = 3
}

/** JSON writer that buffers everything into AST node, returning it afterwards */
class AstJsonWriter extends JsonWriter {
  import AstJsonWriter.*

  // noinspection TypeAnnotation
  private class StackEntry(val entryType: Int) {
    val arrayBuilder = if (entryType == TArray) Vector.newBuilder[JsValue] else null

    val objectOrderBuilder = if (entryType == TObject) Vector.newBuilder[String] else null
    val objectBuilder = if (entryType == TObject) Map.newBuilder[String, JsValue] else null

    val stringBuilder = if (entryType == TString) new mutable.StringBuilder() else null

    var storedResult: Option[JsValue] = None
    var objectKey: String = _

    def pushValue(v: JsValue): Unit =
      entryType match {
        case TObject =>
          objectOrderBuilder += objectKey
          objectBuilder += objectKey -> v
          objectKey = null

        case TArray => arrayBuilder += v
        case TRoot => storedResult = Some(v)

        case TString => throw new IllegalArgumentException("Tried to push data to string value")
      }

    def result(): JsValue =
      entryType match {
        case TObject => new JsObject(objectBuilder.result(), objectOrderBuilder.result())
        case TArray => new JsArray(arrayBuilder.result())
        case TString => JsString(stringBuilder.result())
        case TRoot => storedResult.getOrElse(throw new IllegalStateException("AST writing is not yet finished"))
      }
  }

  private val root: StackEntry = new StackEntry(TRoot)
  private val stateMachine: WriterStateMachine = new WriterStateMachine
  private var states: List[StackEntry] = Nil
  private var state: StackEntry = root

  private def pushState(entryType: Int): Unit = {
    states = state :: states
    state = new StackEntry(entryType)
  }

  def beginArray(): JsonWriter = {
    stateMachine.beginArray()
    pushState(TArray)
    this
  }

  def beginObject(): JsonWriter = {
    stateMachine.beginObject()
    pushState(TObject)
    this
  }

  def beginString(): JsonWriter = {
    stateMachine.beginString()
    pushState(TString)
    this
  }

  def end(): JsonWriter = {
    stateMachine.end()

    val r = state.result()
    state = states.head
    states = states.tail
    state.pushValue(r)

    this
  }

  def key(key: String): JsonWriter = {
    stateMachine.key()
    state.objectKey = key
    this
  }

  def boolValue(value: Boolean): JsonWriter = {
    stateMachine.value()
    state.pushValue(JsBoolean(value))
    this
  }

  def longValue(value: Long): JsonWriter = {
    stateMachine.value()
    state.pushValue(JsLong(value))
    this
  }

  def unsignedLongValue(value: Long): JsonWriter = {
    stateMachine.value()
    val bytes = ByteBuffer.allocate(8).order(ByteOrder.BIG_ENDIAN).putLong(value)
    state.pushValue(JsBigInt(BigInt(1, bytes.array())))
    this
  }

  def doubleValue(value: Double): JsonWriter = {
    stateMachine.value()
    state.pushValue(JsDouble(value))
    this
  }

  def bigintValue(value: BigInt): JsonWriter = {
    stateMachine.value()
    state.pushValue(JsBigInt(value))
    this
  }

  def bigdecValue(value: BigDecimal): JsonWriter = {
    stateMachine.value()
    state.pushValue(JsBigDecimal(value))
    this
  }

  def stringValue(value: String): JsonWriter = {
    if (state.entryType == TString) {
      state.stringBuilder ++= value
      return this
    }

    stateMachine.value()
    state.pushValue(JsString(value))
    this
  }

  def stringValue(value: Array[Char], offset: Int, length: Int): JsonWriter = {
    if (state.entryType == TString) {
      state.stringBuilder.appendAll(value, offset, length)
      return this
    }

    stateMachine.value()
    state.pushValue(JsString(new String(value, offset, length)))
    this
  }

  /** Write null value to stream */
  def nullValue(): JsonWriter = {
    stateMachine.value()
    state.pushValue(JsNull)
    this
  }

  def haveRawChunks: Boolean = false
  def rawChunk(chunk: Array[Char], offset: Int, length: Int): JsonWriter =
    throw new UnsupportedOperationException("AST writer could not handle raw chunks")

  def finish(): Unit = stateMachine.finish()
  def close(): Unit = { /* closing an AST writer is no-op */ }

  /** @return Created AST node */
  def result(): JsValue = root.result()
}
