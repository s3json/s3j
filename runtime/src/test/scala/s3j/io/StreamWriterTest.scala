package s3j.io

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.StringWriter

class StreamWriterTest extends AnyFlatSpec with Matchers {
  private def generateJson(indent: Int, f: JsonWriter => Unit): String = {
    val sw = new StringWriter()
    f(new StreamJsonWriter(sw, indent))
    sw.toString
  }

  it should "serialize root values" in {
    generateJson(0, _.value(true)) shouldBe "true"
    generateJson(0, _.value(1234)) shouldBe "1234"
    generateJson(0, _.value("qwe")) shouldBe "\"qwe\""
    generateJson(0, _.nullValue()) shouldBe "null"
  }

  it should "serialize empty arrays/objects" in {
    generateJson(0, _.beginObject().end()) shouldBe "{}"
    generateJson(2, _.beginObject().end()) shouldBe "{}"
    generateJson(0, _.beginArray().end()) shouldBe "[]"
    generateJson(2, _.beginArray().end()) shouldBe "[]"
  }

  it should "serialize arrays/objects" in {
    generateJson(0, _.beginArray().value(123).value(true).value("qwe").end()) shouldBe
      "[123,true,\"qwe\"]"

    generateJson(2, _.beginArray().value(123).value(true).value("qwe").end()) shouldBe
      "[\n  123,\n  true,\n  \"qwe\"\n]"

    generateJson(0, _.beginObject().key("x").value(123).key("y").nullValue().end()) shouldBe
      "{\"x\":123,\"y\":null}"

    generateJson(2, _.beginObject().key("x").value(123).key("y").nullValue().end()) shouldBe
      "{\n  \"x\": 123,\n  \"y\": null\n}"
  }

  it should "serialize nested arrays/objects" in {
    generateJson(0, _.beginObject().key("x").beginObject().end().key("y").beginObject().key("z")
      .value(true).end().end()) shouldBe "{\"x\":{},\"y\":{\"z\":true}}"

    generateJson(2, _.beginObject().key("x").beginObject().end().key("y").beginObject().key("z")
      .value(true).end().end()) shouldBe "{\n  \"x\": {},\n  \"y\": {\n    \"z\": true\n  }\n}"

    generateJson(0, _.beginArray().beginArray().end().beginArray().value(true).end().end()) shouldBe
      "[[],[true]]"

    generateJson(2, _.beginArray().beginArray().end().beginArray().value(true).end().end()) shouldBe
      "[\n  [],\n  [\n    true\n  ]\n]"
  }

  it should "serialize escaped <script> tag" in {
    generateJson(0, _.value("<script>alert(1);</script>")) shouldBe
      "\"\\u003Cscript>alert(1);\\u003C/script>\""
  }

  it should "serialize escaped strings" in {
    generateJson(0, _.beginObject().key("\u0000\uDDEE").value("\uFFFF").end()) shouldBe
      "{\"\\u0000\\uDDEE\":\"\\uFFFF\"}"

    generateJson(0, _.value("\r\t\n\f\b\\\"")) shouldBe "\"\\r\\t\\n\\f\\b\\\\\\\"\""
  }
}
