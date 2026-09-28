package s3j.format

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import s3j.ast.{JsArray, JsObject, JsValue}
import s3j.format.util.ObjectFormatUtils
import s3j.io.{AstJsonReader, JsonReader, JsonToken}

class ObjectFormatUtilsTest extends AnyFlatSpec with Matchers {
  /** Decode discriminated object the same way as generated enum decoders do */
  private def decodeDiscriminated(reader: JsonReader): (String, JsObject) = {
    val d = ObjectFormatUtils.decodeDiscriminator(reader, "type", 64, allowBuffering = false)
    d should not be null

    val rest = BasicFormats.jsObjectFormat.decode(d.reader)
    ObjectFormatUtils.expectEndObject(reader)
    d.discriminator -> rest
  }

  private def parse(json: String): JsValue = {
    import s3j.io.IoExtensions.fromJson
    json.fromJson[JsValue](using BasicFormats.jsValueFormat)
  }

  it should "decode discriminator from buffered reader" in {
    val reader = new AstJsonReader(parse("""{"type":"A","x":1}"""))
    decodeDiscriminated(reader) shouldBe ("A" -> JsObject("x" -> 1))
  }

  it should "decode out-of-order discriminator from buffered reader without buffering flag" in {
    val reader = new AstJsonReader(parse("""{"x":1,"type":"A","y":true}"""))
    decodeDiscriminated(reader) shouldBe ("A" -> JsObject("x" -> 1, "y" -> true))
  }

  it should "decode discriminator of a singleton object from buffered reader" in {
    val reader = new AstJsonReader(parse("""{"type":"B"}"""))
    decodeDiscriminated(reader) shouldBe ("B" -> JsObject())
  }

  it should "continue reading enclosing structure after buffered discriminator" in {
    val reader = new AstJsonReader(parse("""[{"type":"A"},{"x":2,"type":"B"},3]"""))
    reader.nextToken() shouldBe JsonToken.TArrayStart

    decodeDiscriminated(reader) shouldBe ("A" -> JsObject())
    decodeDiscriminated(reader) shouldBe ("B" -> JsObject("x" -> 2))

    reader.nextToken() shouldBe JsonToken.TNumber
    reader.nextToken() shouldBe JsonToken.TStructureEnd
  }
}
