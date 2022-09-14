package s3j.io

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import s3j.io.util.EscapeUtils

class LexerChunkingTest extends AnyFlatSpec with Matchers {
  import LexerUtils._
  import s3j.io.util.JsonLexer._

  private def chunkingTest(json: String, tokens: Seq[Token], chunkMin: Int, chunkMax: Int, step: Int,
                           context: Int): Unit = {
    val params = TokenSettings(positions = false, collapseContinuations = true, chunkLength = 0,
      contextLength = context)

    for (chunk <- chunkMin to chunkMax by step) {
      collectTokens(json, params.copy(chunkLength = chunk)) shouldBe tokens
    }
  }

  private def chunkingTests(json: String, tokens: Seq[Token]): Unit = {
    chunkingTest(json, tokens, chunkMin = 6, chunkMax = 64, step = 1, context = 0)
    chunkingTest(json, tokens, chunkMin = 8, chunkMax = 128, step = 8, context = 4)
    chunkingTest(json, tokens, chunkMin = 32, chunkMax = 256, step = 32, context = 16)
    chunkingTest(json, tokens, chunkMin = 64, chunkMax = 2048, step = 64, context = 32)
  }

  it should "reliably parse long chunked strings" in {
    val json = "\"" + EscapeUtils.escape(TestConstants.testText) + "\""
    val tokens = Seq(Token(LTString, TestConstants.testText), Token(LTEndOfStream))
    chunkingTests(json, tokens)
  }

  it should "reliably parse long chunked escaped strings" in {
    val json = "\"" + EscapeUtils.escape(TestConstants.testText2) + "\""
    val tokens = Seq(Token(LTString, TestConstants.testText2), Token(LTEndOfStream))
    chunkingTests(json, tokens)
  }

  it should "reliably parse long chunked numbers" in {
    val json = TestConstants.testNumber
    val tokens = Seq(Token(LTNumber, TestConstants.testNumber), Token(LTEndOfStream))
    chunkingTests(json, tokens)
  }

  it should "reliably parse long chunked literals" in {
    val count = 100
    val json = (0 until count).map(_ => "false").mkString("[", ", ", "]")
    val tokens = Seq(Token(LTBeginArray)) ++
      (0 until count-1).flatMap(_ => Seq(Token(LTFalseValue), Token(LTComma))) ++
      Seq(Token(LTFalseValue), Token(LTEndArray), Token(LTEndOfStream))

    chunkingTests(json, tokens)
  }
}
