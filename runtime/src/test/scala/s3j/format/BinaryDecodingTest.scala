package s3j.format

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import s3j.format.impl.BinaryEncoding
import s3j.format.impl.BinaryFormats.ByteArrayFormat
import s3j.io.ChunkingStringReader

import java.util.Base64

class BinaryDecodingTest extends AnyFlatSpec with Matchers {
  private class TestEncoding(val expectedInput: String, val producedOutput: Array[Byte]) extends BinaryEncoding {
    // Non-trivial both binary and coded chunks:
    def byteChunk: Int = 3
    def codedChunk: Int = 4

    private var _inputPos: Int = 0
    private var _outputPos: Int = 0

    def encode(n: Int, in: Array[Byte], inOffset: Int, out: Array[Char]): Int =
      throw new UnsupportedOperationException("TestEncoding.encode")

    def encodeLast(in: Array[Byte], inOffset: Int, bytes: Int, out: Array[Char]): Int =
      throw new UnsupportedOperationException("TestEncoding.encodeLast")

    private def checkExpectation(in: Array[Char], inOffset: Int, length: Int): Unit = {
      for (i <- 0 until length if in(inOffset + i) != expectedInput.charAt(_inputPos + i)) {
        fail(s"input does not match an expectation at pos ${_inputPos + i}: got='${in(inOffset + i)}', " +
          s"expected='" + expectedInput.charAt(_inputPos + i) + "'")
      }

      _inputPos += length
    }

    def decode(n: Int, in: Array[Char], inOffset: Int, out: Array[Byte]): Int = {
      assert(in.length >= n * codedChunk + inOffset)
      assert(out.length >= n * byteChunk)
      assert(_inputPos + n * codedChunk <= expectedInput.length)

      checkExpectation(in, inOffset, n * codedChunk)
      System.arraycopy(producedOutput, _outputPos, out, 0, n * byteChunk)
      _outputPos += n * byteChunk

      n * byteChunk
    }

    def decodeLast(in: Array[Char], inOffset: Int, chars: Int, out: Array[Byte]): Int = {
      assert(in.length >= inOffset + chars)
      assert(chars <= codedChunk)

      checkExpectation(in, inOffset, chars)
      assert(_inputPos == expectedInput.length, "decodeLast didn't complete the input")

      assert(_outputPos >= producedOutput.length - byteChunk)
      val produced = producedOutput.length - _outputPos
      System.arraycopy(producedOutput, _outputPos, out, 0, produced)
      _outputPos = producedOutput.length

      produced
    }

    def verify(): Unit = {
      assert(_inputPos == expectedInput.length, "input wasn't consumed")
      assert(_outputPos == producedOutput.length, "output wasn't produced")
    }
  }

  private def runTest(inputChunks: String*): Unit = {
    assert(inputChunks.nonEmpty)
    val expectedInput = inputChunks.mkString
    val encoding = new TestEncoding(expectedInput, Base64.getDecoder.decode(expectedInput))
    val output = new ByteArrayFormat(encoding).decode(new ChunkingStringReader(inputChunks))
    encoding.verify()
    assert(java.util.Arrays.equals(output, encoding.producedOutput), "output didn't match")
  }

  private def runBase64(inputChunks: String*): Unit = {
    val expectedOutput = Base64.getDecoder.decode(inputChunks.mkString)
    val output = new ByteArrayFormat(BinaryEncoding.Base64).decode(new ChunkingStringReader(inputChunks))
    assert(java.util.Arrays.equals(output, expectedOutput), "output didn't match")

    val output2 = new ByteArrayFormat(BinaryEncoding.Base64).decode(inputChunks.mkString)
    assert(java.util.Arrays.equals(output2, expectedOutput), "output didn't match")
  }

  private def runHex(inputChunks: String*): Unit = {
    val expectedOutput = inputChunks.mkString.grouped(2).map(s => Integer.parseInt(s, 16).toByte).toArray
    val output = new ByteArrayFormat(BinaryEncoding.HexLowercase).decode(new ChunkingStringReader(inputChunks))
    assert(java.util.Arrays.equals(output, expectedOutput), "output didn't match")
  }

  it should "decode empty strings" in {
    runTest("")
  }

  it should "decode complete chunks" in {
    runTest("MEOW")
    runTest("MEOWMEOW")
    runTest("MEOWMEOWMEOW")
  }

  it should "decode incomplete chunks" in {
    runTest("ME")
    runTest("MEO")
    runTest("MEOWME")
    runTest("MEOWMEOWMEO")
  }

  it should "decode split chunks" in {
    runTest("M", "E", "O", "W")
    runTest("ME", "OWME", "OW")
    runTest("ME", "OEMEOWME", "OW")
  }

  it should "decode base64" in {
    runBase64("ME")
    runBase64("MEO")
    runBase64("MEOW")
    runBase64("MEOWMEOWMEOW")
    runBase64("MEOWMEOWMEOWME")
    runBase64("MEOWMEOWMEOWMEO")
    runBase64("+/+/+/MEOW")

    runBase64("M", "E")
    runBase64("ME", "OW")
    runBase64("MEO", "WME", "O")
    runBase64("ME==")
    runBase64("M", "E=", "=")
    runBase64("MEO=")
  }

  it should "decode hex" in {
    runHex("aabb")
    runHex("a", "a", "b", "b")
    runHex("a", "ab", "b")
  }
}
