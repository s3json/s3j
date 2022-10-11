package s3j.format

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import s3j.ast.JsString
import s3j.format.impl.BinaryEncoding
import s3j.format.impl.BinaryFormats.ByteArrayFormat
import s3j.io.AstJsonWriter

import java.util.Base64

class BinaryEncodingTest extends AnyFlatSpec with Matchers {
  private def runEncoding(enc: BinaryEncoding, input: Array[Byte]): String = {
    val astWriter = new AstJsonWriter
    new ByteArrayFormat(enc).encode(astWriter, input)
    astWriter.result().asInstanceOf[JsString].value
  }

  private def runBase64(data: String): Unit = {
    val input = Base64.getDecoder.decode(data)
    val output = Base64.getEncoder.encodeToString(input)
    assert(runEncoding(BinaryEncoding.Base64, input) == output, "output didn't match")
    assert(new ByteArrayFormat(BinaryEncoding.Base64).encode(input) == output, "output didn't match")

    val output2 = Base64.getUrlEncoder.withoutPadding().encodeToString(input)
    assert(runEncoding(BinaryEncoding.Base64Url, input) == output2, "output didn't match")
  }

  private def runHex(data: String): Unit = {
    val input = data.grouped(2).map(s => Integer.parseInt(s, 16).toByte).toArray
    val output = input.map(b => "%02x".format(b & 0xFF)).mkString
    assert(runEncoding(BinaryEncoding.HexLowercase, input) == output, "output didn't match")
  }

  it should "encode base64" in {
    runBase64("ME==")
    runBase64("MEO=")
    runBase64("MEOW")

    runBase64("+/+=")

    runBase64("MEOWME==")
    runBase64("MEOWMEOWMEOW")
  }

  it should "encode hex" in {
    runHex("aabb")
    runHex("00112233")
    runHex("aa1234")
  }
}
