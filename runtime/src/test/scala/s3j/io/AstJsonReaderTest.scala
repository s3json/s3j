package s3j.io

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import s3j.ast.{JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue}
import s3j.format.BasicFormats

import java.io.StringReader
import scala.collection.mutable

class AstJsonReaderTest extends AnyFlatSpec with Matchers {
  private val inputs: Seq[String] = Seq(
    "{}",
    "[]",
    "null",
    "true",
    "false",
    "123",
    "-1.5",
    "\"str\"",
    "\"" + "x" * 5000 + "\"", // longer than a single chunk
    """{"a":[1,{"b":null}],"c":"x","d":true}""",
    """[[],{},[null,false],"s",1]"""
  )

  private def parse(json: String): JsValue =
    BasicFormats.jsValueFormat.decode(new StreamJsonReader(new StringReader(json)))

  /** Read tokens up to the end of stream, merging chunked strings and numbers to be independent of chunk sizes */
  private def readAll(reader: JsonReader): Seq[String] = {
    val result = Vector.newBuilder[String]
    val fragment = new mutable.StringBuilder
    var tokens = 0

    while ({
      tokens += 1
      if (tokens > 10000) fail("reader did not reach end of stream")

      val t = reader.nextToken()
      t match {
        case JsonToken.TStringContinued | JsonToken.TNumberContinued => fragment ++= reader.chunk.toString
        case JsonToken.TString | JsonToken.TNumber =>
          fragment ++= reader.chunk.toString
          result += JsonToken.tokenName(t) + "(" + fragment.result() + ")"
          fragment.clear()

        case JsonToken.TKey => result += "Key(" + reader.key.toString + ")"
        case other => result += JsonToken.tokenName(other)
      }

      t != JsonToken.TEndOfStream
    }) ()

    result.result()
  }

  for (json <- inputs) {
    it should s"produce the same token stream as StreamJsonReader for ${json.take(40)}" in {
      val expected = readAll(new StreamJsonReader(new StringReader(json)))
      val reader = new AstJsonReader(parse(json))

      readAll(reader) shouldBe expected
      expected.last shouldBe JsonToken.tokenName(JsonToken.TEndOfStream)
    }

    it should s"keep returning end of stream after reading ${json.take(40)}" in {
      val reader = new AstJsonReader(parse(json))
      readAll(reader)

      reader.nextToken() shouldBe JsonToken.TEndOfStream
      reader.peekToken shouldBe JsonToken.TEndOfStream
      reader.nextToken() shouldBe JsonToken.TEndOfStream
    }

    it should s"decode ${json.take(40)} and reach end of stream" in {
      val value = parse(json)
      val reader = new AstJsonReader(value)

      BasicFormats.jsValueFormat.decode(reader) shouldBe value
      reader.nextToken() shouldBe JsonToken.TEndOfStream
    }
  }

  it should "reach end of stream after readValue() at root" in {
    val value = JsObject("a" -> JsNumber(1))
    val reader = new AstJsonReader(value)

    reader.readValue() shouldBe value
    reader.nextToken() shouldBe JsonToken.TEndOfStream
    an [IllegalStateException] shouldBe thrownBy { reader.readValue() }
  }

  it should "reach end of stream after readEnclosingValue() of root object" in {
    val value = JsObject("a" -> JsArray(JsBoolean(true)), "b" -> JsString("x"))
    val reader = new AstJsonReader(value)

    reader.nextToken() shouldBe JsonToken.TObjectStart
    reader.readEnclosingValue() shouldBe value
    reader.nextToken() shouldBe JsonToken.TEndOfStream
  }

  it should "reach end of stream after scalar root values" in {
    for (value <- Seq(JsNull, JsBoolean(true), JsBoolean(false))) {
      val reader = new AstJsonReader(value)
      reader.nextToken() should not be JsonToken.TEndOfStream
      reader.nextToken() shouldBe JsonToken.TEndOfStream
      reader.nextToken() shouldBe JsonToken.TEndOfStream
    }
  }
}
