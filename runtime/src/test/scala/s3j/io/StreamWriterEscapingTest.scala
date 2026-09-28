package s3j.io

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import s3j.JsPath
import s3j.format.BasicFormats
import s3j.io.IoExtensions.toJsonString
import s3j.io.StreamJsonWriter.WriterSettings

import java.io.{StringReader, StringWriter}

class StreamWriterEscapingTest extends AnyFlatSpec with Matchers {
  private val Minimal = WriterSettings(escapeHtml = false, escapeUnicode = false)
  private val NoHtml = WriterSettings(escapeHtml = false)
  private val NoUnicode = WriterSettings(escapeUnicode = false)
  private val Ascii = WriterSettings(escapeEightBit = true)
  private val AsciiNoHtml = WriterSettings(escapeHtml = false, escapeUnicode = false, escapeEightBit = true)

  private val AllSettings = Seq(WriterSettings(), Minimal, NoHtml, NoUnicode, Ascii, AsciiNoHtml)

  private def render(settings: WriterSettings)(f: JsonWriter => Unit): String = {
    val sw = new StringWriter()
    f(new StreamJsonWriter(sw, settings))
    sw.toString
  }

  /** @return Contents of JSON string literal, as written by `stringValue(String)` */
  private def escaped(settings: WriterSettings, str: String): String = {
    val r = render(settings)(_.stringValue(str))
    r.head shouldBe '"'
    r.last shouldBe '"'
    r.substring(1, r.length - 1)
  }

  private val Emoji = "😀"

  it should "always escape characters mandated by RFC 8259" in {
    for (s <- AllSettings) {
      escaped(s, "\"\\") shouldBe "\\\"\\\\"
      escaped(s, "\r\t\n\f\b") shouldBe "\\r\\t\\n\\f\\b"
      escaped(s, "\u0000\u0001\u001F") shouldBe "\\u0000\\u0001\\u001F"
    }
  }

  it should "never escape printable ASCII except HTML-sensitive characters" in {
    val printable = (' ' to '~').filterNot(c => "\"\\<>&".contains(c)).mkString
    for (s <- AllSettings) escaped(s, printable) shouldBe printable
  }

  it should "escape HTML-sensitive characters by default" in {
    escaped(WriterSettings(), "<a href='x'>&</a>") shouldBe "\\u003Ca href='x'\\u003E\\u0026\\u003C/a\\u003E"
    escaped(WriterSettings(), "\u2028\u2029") shouldBe "\\u2028\\u2029"
  }

  it should "not escape HTML-sensitive characters when disabled" in {
    escaped(NoHtml, "<a>&</a>") shouldBe "<a>&</a>"
    escaped(Minimal, "<a>&</a>") shouldBe "<a>&</a>"

    // Line separators are also discretionary, so they are escaped as long as any of the two classes is enabled:
    escaped(NoHtml, "\u2028") shouldBe "\\u2028"
    escaped(NoUnicode, "\u2028") shouldBe "\\u2028"
    escaped(Minimal, "\u2028\u2029") shouldBe "\u2028\u2029"
  }

  it should "escape discretionary unicode characters by default" in {
    escaped(WriterSettings(), "\u007F\u00A0\u200B\uFEFF\uFFFF") shouldBe "\\u007F\\u00A0\\u200B\\uFEFF\\uFFFF"
    escaped(WriterSettings(), Emoji) shouldBe "\\uD83D\\uDE00"
    escaped(WriterSettings(), "\uDDEE") shouldBe "\\uDDEE"
  }

  it should "not escape discretionary unicode characters when disabled" in {
    val msg = "\u007F\u00A0\u200B\uFEFF\uFFFF"

    for (s <- Seq(NoUnicode, Minimal)) {
      escaped(s, msg) shouldBe msg
      escaped(s, Emoji) shouldBe Emoji
    }
  }

  it should "keep letters and digits of any script unescaped unless 8-bit escaping is enabled" in {
    val text = "é привет 日本 ٣"
    for (s <- Seq(WriterSettings(), Minimal, NoHtml, NoUnicode)) escaped(s, text) shouldBe text

    for (s <- Seq(Ascii, AsciiNoHtml)) {
      escaped(s, text) shouldBe
        "\\u00E9 \\u043F\\u0440\\u0438\\u0432\\u0435\\u0442 \\u65E5\\u672C \\u0663"
    }
  }

  it should "produce pure ASCII output with 8-bit escaping" in {
    val text = "\u00E9<\u2028>" + Emoji + "\u00A0\u007F"
    escaped(Ascii, text) shouldBe "\\u00E9\\u003C\\u2028\\u003E\\uD83D\\uDE00\\u00A0\\u007F"
    escaped(AsciiNoHtml, text) shouldBe "\\u00E9<\\u2028>\\uD83D\\uDE00\\u00A0\\u007F"
  }

  it should "apply escaping settings to keys and all string writing methods" in {
    val text = "<é>" + Emoji
    val chars = text.toCharArray

    for (s <- AllSettings) {
      val expected = "\"" + escaped(s, text) + "\""

      render(s)(_.beginObject().key(text).nullValue().end()) shouldBe "{" + expected + ":null}"
      render(s)(_.stringValue(chars, 0, chars.length)) shouldBe expected
      render(s)(_.beginArray().stringValue(chars, 0, chars.length).end()) shouldBe "[" + expected + "]"

      // String mode, with value split between string chunks and raw chunks:
      render(s)(_.beginString().stringValue(text.substring(0, 2)).rawChunk(chars, 2, chars.length - 2).end()) shouldBe
        expected
    }
  }

  it should "produce output that decodes to the original string with any settings" in {
    val samples = Seq(
      "", "plain", "\"quoted\\\"", "\u0000\u001F\u007F", "<script>&</script>", "\u2028\u2029",
      "é привет 日本", Emoji, "\uDDEE\uD83D", "\u00a0\u200b\ufeff\uffff", "a/b"
    )

    for (s <- AllSettings; sample <- samples) {
      val json = render(s)(_.stringValue(sample))
      withClue(s"settings=$s, json=$json: ") {
        BasicFormats.stringFormat.decode(new StreamJsonReader(new StringReader(json))) shouldBe sample
      }
    }
  }

  it should "use default escaping in toJsonString" in {
    "<é>".toJsonString(using BasicFormats.stringFormat) shouldBe "\"\\u003Cé\\u003E\""
  }

  it should "display path keys readably" in {
    JsPath.obj("ключ").toString shouldBe "$.ключ"
    JsPath.obj("ключ с пробелом").toString shouldBe "$[\"ключ с пробелом\"]"
    JsPath.obj("a<b>&c").toString shouldBe "$[\"a<b>&c\"]"
    JsPath.obj("q\"\n\u00a0").toString shouldBe "$[\"q\\\"\\n\\u00A0\"]"
  }
}
