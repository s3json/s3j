package s3j.io

import s3j.io.util.JsonLexer
import java.io.StringReader
import scala.collection.mutable

object LexerUtils {
  case class Token(t: Int, chunk: String = "", pos: Option[StreamPosition] = None) {
    override def toString: String =
      if (dataTokens(t)) tokenNames(t) + "(" + chunk + ")"
      else tokenNames(t)
  }

  case class TokenSettings(
    /** Whether to return token positions */
    positions: Boolean,

    /** Whether to collapse continuation tokens into a single long token */
    collapseContinuations: Boolean,

    /** Lexer chunk length */
    chunkLength: Int,

    /** Lexer context length */
    contextLength: Int
  )

  /** Human-readable names of lexer tokens */
  val tokenNames: Array[String] = Array(
    "EndOfStream", "BeginObject", "BeginArray", "EndObject", "EndArray", "Comma", "Colon",
    "TrueValue", "FalseValue", "NullValue", "StringContinued", "String", "NumberContinued", "Number"
  )

  /** Set of token for which data chunk is meaningful upon return */
  val dataTokens: Set[Int] = Set(
    JsonLexer.LTString, JsonLexer.LTStringContinued, JsonLexer.LTNumber, JsonLexer.LTNumberContinued
  )

  /** Set of tokens which are continued */
  val continuationTokens: Set[Int] = Set(JsonLexer.LTStringContinued, JsonLexer.LTNumberContinued)

  /** Parse tokens and dump them as table */
  def dumpTokens(json: String, chunkLength: Int = 64, contextLength: Int = 4): Unit = {
    val lexer = new JsonLexer(new StringReader(json), chunkLength, contextLength)
    println("               Token Offs Lin Col   Data chunk")
    println("==================== ==== === ===   ==========")

    var token: Int = -1
    while (token != JsonLexer.LTEndOfStream) {
      token = lexer.readToken()
      val chunk = if (dataTokens(token)) lexer.chunk.toString else ""
      val pos = lexer.position

      println("%20s %4d %3d %3d   %s".format(tokenNames(token), pos.offset, pos.line, pos.column, chunk))
    }
  }

  /** Parse all tokens to sequence */
  def collectTokens(json: String, settings: TokenSettings): Seq[Token] = {
    val lexer = new JsonLexer(new StringReader(json), settings.chunkLength, settings.contextLength)
    val r = Vector.newBuilder[Token]

    var token: Int = -1
    var tokenValid: Boolean = false

    while (token != JsonLexer.LTEndOfStream) {
      if (!tokenValid) token = lexer.readToken()
      else tokenValid = false

      val pos = if (settings.positions) Some(lexer.position) else None

      val chunk = {
        if (!dataTokens(token)) ""
        else if (!settings.collapseContinuations || !continuationTokens(token)) lexer.chunk.toString
        else {
          val sb = new mutable.StringBuilder()
          sb ++= lexer.chunk.toString
          while (continuationTokens(token)) {
            token = lexer.readToken()
            sb ++= lexer.chunk.toString
          }
          sb.result()
        }
      }

      r += Token(token, chunk, pos)
    }

    r.result()
  }
}
