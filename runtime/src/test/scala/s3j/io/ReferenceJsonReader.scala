package s3j.io

import s3j.JsPath
import s3j.io.ReferenceJsonReader.Token

import scala.collection.mutable

object ReferenceJsonReader {
  case class Token(loc: JsonLocation, t: JsonToken, chunk: String = "") {
    override def toString(): String = {
      val sb = new mutable.StringBuilder()
      sb ++= JsonToken.tokenName(t) ++= "["

      loc match {
        case JsonLocation.StreamLocation(pos, path) =>
          sb ++= "%d,%d:%d; %s".format(pos.offset, pos.line, pos.column, path.toString)

        case JsonLocation.TreeLocation(path) =>
          sb ++= "%s".format(path.toString)

        case _ => sb ++= "?"
      }

      sb ++= "]"

      if (dataTokens(t) || t == JsonToken.TKey) sb ++= "(" ++= chunk ++= ")"

      sb.result()
    }
  }

  private val dataTokens = Set(JsonToken.TString, JsonToken.TStringContinued, JsonToken.TNumber,
    JsonToken.TNumberContinued)

  private val continuationTokens = Set(JsonToken.TStringContinued, JsonToken.TNumberContinued)

  def gatherTokens(reader: JsonReader): Seq[Token] = {
    val r = Vector.newBuilder[Token]

    var token: JsonToken = -1
    while (token != JsonToken.TEndOfStream) {
      token = reader.nextToken()
      val location = reader.location

      val chunk: String =
        if (token == JsonToken.TKey) reader.key.toString
        else if (!dataTokens(token)) ""
        else if (!continuationTokens(token)) reader.chunk.toString
        else {
          val sb = new mutable.StringBuilder()
          sb ++= reader.chunk.toString

          while (continuationTokens(token)) {
            token = reader.nextToken()
            sb ++= reader.chunk.toString
          }

          sb.result()
        }

      r += Token(location, token, chunk)
    }

    r.result()
  }
}

/** Very simple and straightforward JSON reader to get reference results */
class ReferenceJsonReader(str: String, contextLength: Int) {
  private var position = 0
  private var lineNumber = 1
  private var columnNumber = 0

  private var path: JsPath = JsPath.Root
  private val tokensBuilder = Vector.newBuilder[Token]

  private def head: Char = if (position < str.length) str.charAt(position) else '\u0000'

  private def location: JsonLocation.StreamLocation = {
    val contextStart = math.max(0, position - contextLength)
    val contextEnd = math.min(str.length, position + contextLength)
    val context = str.substring(contextStart, contextEnd)

    JsonLocation.StreamLocation(StreamPosition(position, lineNumber, columnNumber, context, position - contextStart),
      path)
  }

  val result: Vector[Token] = { readRoot(); tokensBuilder.result() }

  private def get(): Char = {
    val r = str.charAt(position)
    position += 1
    columnNumber += 1

    if (r == '\n') {
      lineNumber += 1
      columnNumber = 0
    }

    r
  }

  private def skipWhitespace(): Unit = {
    var c: Char = 0
    while ({ c = head; c == '\n' || c == '\r' || c == '\t' || c == ' ' }) get()
  }

  private def readString(key: Boolean): Unit = {
    val startLocation = location
    if (get() != '\"') throw new IllegalArgumentException("string start expected")
    val sb = new mutable.StringBuilder()

    while (head != '\"') {
      sb += (get() match {
        case '\\' => get() match {
          case 'r' => '\r'
          case 'n' => '\n'
          case 't' => '\t'
          case 'b' => '\b'
          case 'f' => '\f'
          case '/' => '/'
          case '\"' => '\"'
          case '\'' => '\''
          case '\\' => '\\'
          case 'u' => Integer.parseInt(get().toString + get() + get() + get(), 16).toChar
        }

        case other => other
      })
    }

    if (get() != '\"') throw new IllegalArgumentException("string end expected")
    val result = sb.result()
    if (key) path = JsPath.Object(path, result)
    tokensBuilder += Token(startLocation.copy(path = path), if (key) JsonToken.TKey else JsonToken.TString, result)
  }

  private def readNumber(): Unit = {
    val startLocation = location
    val sb = new mutable.StringBuilder()

    def isNumber(c: Char): Boolean = (c >= '0' && c <= '9') || c == '+' || c == '-' || c == '.' || c == 'e' || c == 'E'
    while (isNumber(head)) sb += get()

    tokensBuilder += Token(startLocation, JsonToken.TNumber, sb.result())
  }

  private def readLiteral(lit: String, token: Int): Unit = {
    val startLocation = location
    if (str.substring(position, position + lit.length) != lit) {
      throw new IllegalArgumentException("expected literal " + lit)
    }

    tokensBuilder += Token(startLocation, token)
    for (_ <- lit.indices) get()
  }

  private def readObject(): Unit = {
    tokensBuilder += Token(location, JsonToken.TObjectStart)
    if (get() != '{') throw new IllegalArgumentException("object start expected")
    skipWhitespace()

    if (head != '}') {
      var finished = false
      while (!finished) {
        readString(key = true)

        skipWhitespace()
        if (get() != ':') throw new IllegalArgumentException("expected ':'")
        skipWhitespace()

        readValue()
        skipWhitespace()

        path = path.asInstanceOf[JsPath.Object].parent

        if (head == ',') { get(); skipWhitespace() }
        else finished = true
      }
    }

    tokensBuilder += Token(location, JsonToken.TStructureEnd)
    if (get() != '}') throw new IllegalArgumentException("object end expected")
  }

  private def readArray(): Unit = {
    tokensBuilder += Token(location, JsonToken.TArrayStart)
    if (get() != '[') throw new IllegalArgumentException("object start expected")
    skipWhitespace()

    if (head != ']') {
      var finished = false
      var index = 0
      while (!finished) {
        path = JsPath.Array(path, index)

        readValue()
        skipWhitespace()

        path = path.asInstanceOf[JsPath.Array].parent
        index += 1

        if (head == ',') { get(); skipWhitespace() }
        else finished = true
      }
    }

    tokensBuilder += Token(location, JsonToken.TStructureEnd)
    if (get() != ']') throw new IllegalArgumentException("array end expected")
  }

  private def readValue(): Unit = {
    skipWhitespace()

    head match {
      case '\"' => readString(key = false)
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' => readNumber()
      case 't' => readLiteral("true", JsonToken.TTrueValue)
      case 'f' => readLiteral("false", JsonToken.TFalseValue)
      case 'n' => readLiteral("null", JsonToken.TNullValue)
      case '[' => readArray()
      case '{' => readObject()
    }
  }

  private def readRoot(): Unit = {
    readValue()
    skipWhitespace()

    if (position != str.length) throw new IllegalArgumentException("Expected end of stream")
    tokensBuilder += Token(location, JsonToken.TEndOfStream)
  }
}
