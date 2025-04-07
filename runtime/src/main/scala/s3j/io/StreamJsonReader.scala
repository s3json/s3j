package s3j.io

import s3j.io.StreamJsonReader._
import s3j.io.util.{CharRange, JsPathBuilder, JsonLexer}
import s3j.io.util.JsonLexer._

import java.io.Reader
import scala.annotation.switch

object StreamJsonReader {
  case class ReaderSettings(chunkLength: Int, contextLength: Int, maxKeyLength: Int, maxNesting: Int)

  val defaultSettings: ReaderSettings = ReaderSettings(1024, 64, 8192, 512)

  private final val SRoot           = 0
  private final val SRootCompleted  = 1
  private final val SObjectInit     = 2
  private final val SObjectKey      = 3
  private final val SObjectColon    = 4
  private final val SObjectValue    = 5
  private final val SObjectComma    = 6
  private final val SArrayInit      = 7
  private final val SArrayValue     = 8
  private final val SArrayComma     = 9
  private final val SDataChunk      = 10
}

class StreamJsonReader(in: Reader, settings: ReaderSettings = StreamJsonReader.defaultSettings) extends JsonReader {
  private val lexer: JsonLexer = new JsonLexer(in, settings.chunkLength, settings.contextLength)
  private val pathBuilder: JsPathBuilder = new JsPathBuilder(settings.maxKeyLength, settings.maxNesting)

  private var state: Int = SRoot
  private var savedState: Int = 0 // previous state for SDataChunk

  private def unexpectedToken(token: Int, expected: String): Nothing =
    parseError("unexpected token " + JsonLexer.tokenName(token) + ", expected " + expected)

  inline def readValue(): JsonToken = processValue(lexer.readToken())

  private def processValue(t: Int): JsonToken = t match {
    case LTBeginObject =>
      pathBuilder.pushObject()
      state = SObjectInit
      JsonToken.TObjectStart

    case LTBeginArray =>
      pathBuilder.pushArray()
      state = SArrayInit
      JsonToken.TArrayStart

    case LTTrueValue  => JsonToken.TTrueValue
    case LTFalseValue => JsonToken.TFalseValue
    case LTNullValue  => JsonToken.TNullValue

    case LTString =>
      if (state == SDataChunk) state = savedState
      JsonToken.TString

    case LTNumber =>
      if (state == SDataChunk) state = savedState
      JsonToken.TNumber

    case LTStringContinued =>
      if (state != SDataChunk) {
        savedState = state
        state = SDataChunk
      }

      JsonToken.TStringContinued

    case LTNumberContinued =>
      if (state != SDataChunk) {
        savedState = state
        state = SDataChunk
      }

      JsonToken.TNumberContinued

    case other => unexpectedToken(other, "any value")
  }

  private def finishKey(): JsonToken = {
    pathBuilder.appendKey(lexer.chunk)
    pathBuilder.finishLevel()
    state = SObjectColon
    JsonToken.TKey
  }

  private def popStructure(): JsonToken = {
    pathBuilder.pop() match {
      case JsPathBuilder.RRoot => state = SRootCompleted
      case JsPathBuilder.RObject => state = SObjectComma
      case JsPathBuilder.RArray => state = SArrayComma
    }

    JsonToken.TStructureEnd
  }

  /** @return Next token from the stream */
  protected def readToken(): JsonToken = {
    while (true) {
      (state: @switch) match {
        case SRoot =>
          state = SRootCompleted
          return readValue()

        case SRootCompleted => lexer.readToken() match {
          case LTEndOfStream => return JsonToken.TEndOfStream
          case other => unexpectedToken(other, "end of stream")
        }

        case SObjectInit => lexer.readToken() match {
          case LTEndObject => return popStructure()
          case LTStringContinued => pathBuilder.appendKey(lexer.chunk)
          case LTString => return finishKey()
          case other => unexpectedToken(other, "object key (or end of object)")
        }

        case SObjectKey => lexer.readToken() match {
          case LTStringContinued => pathBuilder.appendKey(lexer.chunk)
          case LTString => return finishKey()
          case other => unexpectedToken(other, "object key (a string)")
        }

        case SObjectColon => lexer.readToken() match {
          case LTColon => state = SObjectValue
          case other => unexpectedToken(other, "key-value separator (a colon)")
        }

        case SObjectValue =>
          state = SObjectComma
          return readValue()

        case SObjectComma => lexer.readToken() match {
          case LTComma =>
            pathBuilder.resetKey()
            state = SObjectKey

          case LTEndObject => return popStructure()
          case other => unexpectedToken(other, "comma (or end of object)")
        }

        case SArrayInit => lexer.readToken() match {
          case LTEndArray => return popStructure()
          case other =>
            state = SArrayComma
            pathBuilder.incrementIndex()
            return processValue(other)
        }

        case SArrayValue =>
          state = SArrayComma
          pathBuilder.incrementIndex()
          return readValue()

        case SArrayComma => lexer.readToken() match {
          case LTComma => state = SArrayValue
          case LTEndArray => return popStructure()
          case other => unexpectedToken(other, "comma (or end of array)")
        }

        case SDataChunk => return readValue()
      }
    }

    throw new RuntimeException("Unreachable")
  }

  def chunk: CharRange = lexer.chunk
  def key: KeyHandle = pathBuilder.key
  def location: JsonLocation = JsonLocation.StreamLocation(lexer.position, pathBuilder.toPath)
}
