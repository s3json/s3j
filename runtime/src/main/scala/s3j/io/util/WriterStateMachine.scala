package s3j.io.util

object WriterStateMachine {
  type State = Int

  final val SRoot         = 0
  final val SRootFinished = 1
  final val SObjectKey    = 2
  final val SObjectValue  = 3
  final val SArrayValue   = 4
  final val SString       = 5

  private val _stateNames: Array[String] =
    Array("Root", "RootFinished", "ObjectKey", "ObjectValue", "ArrayValue", "String")
}

/** Utility class to ensure correctness of writer invocation */
class WriterStateMachine {
  import WriterStateMachine.*

  private var _state: State = SRoot
  private var _stateStack: List[State] = Nil

  private def stateName: String = _stateNames(_state)

  // State machine to ensure that only correct JSON can be built with this writer

  private def checkBegin(what: String, newState: State): Unit = {
    value()
    _stateStack = _state :: _stateStack
    _state = newState
  }

  def beginArray(): Unit = checkBegin("array", SArrayValue)

  def beginObject(): Unit = checkBegin("object", SObjectKey)

  def beginString(): Unit = {
    value()
    checkBegin("string", SString)
  }

  def value(): Unit = {
    if (_state != SObjectValue && _state != SArrayValue && _state != SRoot)
      throw new IllegalStateException(s"Could not write value in state $stateName")

    if (_state == SObjectValue)
      _state = SObjectKey

    if (_state == SRoot)
      _state = SRootFinished
  }

  def end(): Unit = {
    if (_state != SArrayValue && _state != SObjectKey && _state != SString)
      throw new IllegalStateException(s"Could not finish current record in state $stateName")

    _state = _stateStack.head
    _stateStack = _stateStack.tail
  }

  def key(): Unit = {
    if (_state != SObjectKey)
      throw new IllegalStateException(s"Could not write key in state $stateName")

    _state = SObjectValue
  }

  def finish(): Unit = {
    if (_state != SRootFinished)
      throw new IllegalStateException(s"Could not close writer in state $stateName: document will be incomplete")
  }
}
