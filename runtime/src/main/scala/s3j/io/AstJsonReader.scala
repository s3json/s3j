package s3j.io

import s3j.JsPath
import s3j.ast.{JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue}
import s3j.io.JsonToken.TEndOfStream
import s3j.io.util.CharRange

private[io] object AstJsonReader {
  final val NObject   = 0
  final val NArray    = 1
  final val NString   = 2
  final val NNumber   = 3
}

class AstJsonReader(node: JsValue, locationPrefix: JsPath = JsPath.Root, chunkLength: Int = 1024)
extends JsonReader with JsonReader.Buffered {
  import AstJsonReader._

  private class StackEntry(val node: JsValue) {
    val nodeType: Int = node match {
      case _: JsObject  => NObject
      case _: JsArray   => NArray
      case _: JsString  => NString
      case _: JsNumber  => NNumber
      case _ => throw new RuntimeException("StackEntry with unsupported node type: " + node.getClass)
    }

    var keysIterator: Iterator[String] = node match {
      case obj: JsObject => obj.keysIterator
      case _ => null
    }

    val arrayIterator: Iterator[JsValue] = node match {
      case arr: JsArray => arr.iterator
      case _ => null
    }

    val string: String = node match {
      case JsString(value) => value
      case n: JsNumber => n.toString
      case _ => null
    }

    var currentKey: String = _
    var currentIndex: Int = -1

    var pendingValue: JsValue | Null = null
    val stringLength: Int = if (string != null) string.length else 0
    var stringOffset: Int = 0
  }

  private var states: List[StackEntry] = Nil
  private var state: StackEntry = _
  private var isCompleted: Boolean = false
  private var keyInstance: KeyHandle.StringHandle = new KeyHandle.StringHandle

  val chunk: CharRange = new CharRange(chunkLength)
  def key: KeyHandle = keyInstance

  private def pushState(value: JsValue): Unit = {
    if (state != null) {
      states = state :: states
    }

    state = new StackEntry(value)
  }

  private def popState(): Unit =
    if (states.isEmpty) {
      state = null
      isCompleted = true
    } else {
      state = states.head
      states = states.tail
    }

  /**
   * Refill char buffer from string-based values.
   * @return true if string will be continued
   */
  private def refillRange(): Boolean = {
    val toWrite = math.min(chunk.capacity, state.stringLength - state.stringOffset)

    chunk.clear().put(state.string, state.stringOffset, toWrite).flip()
    state.stringOffset += toWrite

    state.stringOffset < state.stringLength
  }

  private def emitValue(value: JsValue): JsonToken =
    value match {
      case _: JsObject =>
        pushState(value)
        JsonToken.TObjectStart

      case _: JsArray =>
        pushState(value)
        JsonToken.TArrayStart

      case JsNull =>
        JsonToken.TNullValue

      case JsBoolean(v) =>
        if (v) JsonToken.TTrueValue else JsonToken.TFalseValue

      case _: JsString =>
        pushState(value)
        if (refillRange()) JsonToken.TStringContinued
        else { popState(); JsonToken.TString }

      case _: JsNumber =>
        pushState(value)
        if (refillRange()) JsonToken.TNumberContinued
        else { popState(); JsonToken.TNumber }
    }

  /** @return Next token from the stream */
  protected def readToken(): JsonToken = {
    if (state == null) {
      // very first token - start state machine with root node
      return emitValue(node)
    }

    if (isCompleted) {
      return TEndOfStream
    }

    if (state.pendingValue != null) {
      val pending = state.pendingValue
      state.pendingValue = null
      return emitValue(pending)
    }

    state.nodeType match {
      case NObject =>
        if (!state.keysIterator.hasNext) {
          popState()
          return JsonToken.TStructureEnd
        }

        state.currentKey = state.keysIterator.next()
        state.pendingValue = state.node.asInstanceOf[JsObject](state.currentKey)
        keyInstance.str = state.currentKey
        JsonToken.TKey

      case NArray =>
        if (!state.arrayIterator.hasNext) {
          popState()
          return JsonToken.TStructureEnd
        }

        state.currentIndex += 1
        emitValue(state.arrayIterator.next())

      case NString =>
        if (refillRange()) JsonToken.TStringContinued
        else { popState(); JsonToken.TString }

      case NNumber =>
        if (refillRange()) JsonToken.TNumberContinued
        else { popState(); JsonToken.TNumber }
    }
  }

  def location: JsonLocation = {
    val path = (state :: states).reverseIterator.foldLeft(locationPrefix) {
      case (p, e) if e.nodeType == NObject =>
        if (e.currentKey != null) JsPath.Object(p, e.currentKey)
        else p

      case (p, e) if e.nodeType == NArray =>
        if (e.currentIndex >= 0) JsPath.Array(p, e.currentIndex)
        else p

      case (p, _) => p
    }

    JsonLocation.TreeLocation(path)
  }

  def enclosingValue: JsValue = {
    if (state == null || isCompleted) {
      throw new IllegalStateException("No enclosing value is currently present")
    }

    state.node
  }

  def readEnclosingValue(): JsValue = {
    val r = enclosingValue
    popState()
    /* return */ r
  }

  def readValue(): JsValue = {
    if (state == null) {
      isCompleted = true
      return node
    }

    if (isCompleted) {
      throw new IllegalStateException("readValue() at the end of stream")
    }

    if (state.pendingValue != null) {
      val r = state.pendingValue
      state.pendingValue = null
      return r
    }

    state.nodeType match {
      case NObject =>
        // readKey() should do all hard work in advancing the state machine and leave for us only pendingValue
        throw new IllegalStateException("readValue() when object was reading a key")

      case NArray =>
        if (state.arrayIterator.hasNext) state.arrayIterator.next()
        else throw new IllegalStateException("readValue() reached the end of array")

      case NString => throw new IllegalStateException("readValue() inside of string; use readEnclosingValue()")
      case NNumber => throw new IllegalStateException("readValue() inside of number; use readEnclosingValue()")
    }
  }

  def remainingKeys: Seq[String] = {
    if (state == null || isCompleted) {
      throw new IllegalStateException("No enclosing value is currently present")
    }

    if (state.nodeType != NObject) {
      throw new IllegalStateException("remainingKeys when not in object node")
    }

    val result = state.keysIterator.toVector
    state.keysIterator = result.iterator // recover consumed iterator

    result
  }
}
