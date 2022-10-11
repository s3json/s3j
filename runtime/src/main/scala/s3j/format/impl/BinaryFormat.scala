package s3j.format.impl

import s3j.format.util.DecoderUtils
import s3j.format.{CombinedFormats, JsonDecoder, JsonEncoder, JsonFormat, StringyDecoder, StringyEncoder, StringyFormat}
import s3j.io.{JsonReader, JsonToken, JsonWriter, Scratchpad}

import scala.collection.mutable

abstract class BinaryFormat[T](encoding: BinaryEncoding) extends CombinedFormats.StringyJsonFormat[T] { outer =>
  protected type Iterator
  protected type Builder

  private val nChunks: Int = math.min(2048 / encoding.byteChunk, 1024 / encoding.codedChunk)
  private val byteChunk: Int = nChunks * encoding.byteChunk
  private val codedChunk: Int = nChunks * encoding.codedChunk

  /** @return Fresh builder instance to construct the output */
  protected def newBuilder: Builder

  /** @return Fresh builder instance initialized with expected size */
  protected def newBuilder(expectedSize: Int): Builder

  /** Update builder state to accommodate new chunk of data. Chunk will be overwritten later. */
  protected def appendChunk(b: Builder, chunk: Array[Byte], offset: Int, length: Int): Unit

  /** @return Final result based on builder state */
  protected def result(b: Builder): T

  /** @return Fresh iterator to consume input */
  protected def newIterator(value: T): Iterator

  /** @return Byte length of the value */
  protected def valueLength(value: T): Int

  /**
    * @return Number of read bytes, or 0 when end is reached. Iterator must always fill entire buffer when data is
   *         available, otherwise incorrect results might be produced.
   */
  protected def readChunk(it: Iterator, target: Array[Byte], offset: Int, length: Int): Int

  // ---

  private def newSizedBuilder(codedSize: Int): Builder =
    newBuilder((codedSize + encoding.codedChunk - 1) / encoding.codedChunk * encoding.byteChunk)

  private def computeIterChunks(sp: Scratchpad): Int =
    math.min(sp.bytes.length / encoding.byteChunk, sp.chars.length / encoding.codedChunk)

  protected def performEncoding(value: T, write: (Array[Char], Int, Int) => Unit): Unit = {
    val it = newIterator(value)
    val sp = Scratchpad.get()

    val iterChunks = computeIterChunks(sp)
    val iterBytes = iterChunks * encoding.byteChunk
    var r = 0

    while ({ r = readChunk(it, sp.bytes, 0, iterBytes); r > 0 }) {
      if (r == iterBytes) {
        val coded = encoding.encode(iterChunks, sp.bytes, 0, sp.chars)
        write(sp.chars, 0, coded)
      } else {
        val chunks = r / encoding.byteChunk

        if (chunks > 0) {
          val coded = encoding.encode(chunks, sp.bytes, 0, sp.chars)
          write(sp.chars, 0, coded)
        }

        val rr = encoding.byteChunk * chunks
        if (rr < r) {
          val coded = encoding.encodeLast(sp.bytes, rr, r - rr, sp.chars)
          write(sp.chars, 0, coded)
        }
      }
    }
  }

  def encode(writer: JsonWriter, value: T): Unit = {
    writer.beginString()
    performEncoding(value, writer.stringValue)
    writer.end()
  }

  def encode(value: T): String = {
    val sb = new mutable.StringBuilder()
    performEncoding(value, sb.appendAll)
    sb.result()
  }

  private def decodeStream(reader: JsonReader): T = {
    val bld = newBuilder

    val sp = Scratchpad.get()
    val byteChunks = sp.bytes.length / encoding.byteChunk
    var spChars = 0

    var continue = true
    var last = false

    while (continue) {
      val chunk = reader.chunk

      if (spChars > 0) {
        val toFill = math.min(encoding.codedChunk - spChars, chunk.remaining)
        chunk.get(sp.chars, spChars, toFill)
        spChars += toFill
      }

      if (spChars == encoding.codedChunk) {
        val r = encoding.decode(1, sp.chars, 0, sp.bytes)
        appendChunk(bld, sp.bytes, 0, r)
        spChars = 0
      }

      var nChunks = math.min(chunk.remaining / encoding.codedChunk, byteChunks)
      while (nChunks > 0) {
        val r = encoding.decode(nChunks, chunk.data, chunk.position, sp.bytes)
        appendChunk(bld, sp.bytes, 0, r)
        chunk.adjustPosition(nChunks * encoding.codedChunk)
        nChunks = math.min(chunk.remaining / encoding.codedChunk, byteChunks)
      }

      val rr = chunk.remaining
      if (rr > 0) {
        chunk.get(sp.chars, spChars, rr)
        spChars += rr
      }

      if (last) {
        if (spChars > 0) {
          val r = encoding.decodeLast(sp.chars, 0, spChars, sp.bytes)
          appendChunk(bld, sp.bytes, 0, r)
        }

        continue = false
      } else reader.nextToken() match {
        case JsonToken.TStringContinued => /* continue */
        case JsonToken.TString => last = true
        case other => DecoderUtils.throwUnexpected(reader, "a string", other)
      }
    }

    result(bld)
  }

  def decode(reader: JsonReader): T = reader.nextToken() match {
    case JsonToken.TString =>
      val chunk = reader.chunk
      val bld = newSizedBuilder(chunk.remaining)
      val sp = Scratchpad.get()

      val nChunks = chunk.remaining / encoding.codedChunk
      if (nChunks > 0) {
        val decoded = encoding.decode(nChunks, chunk.data, chunk.position, sp.bytes)
        appendChunk(bld, sp.bytes, 0, decoded)
      }

      val rr = nChunks * encoding.codedChunk
      if (rr < chunk.remaining) {
        val decoded = encoding.decodeLast(chunk.data, chunk.position + rr, chunk.remaining - rr, sp.bytes)
        appendChunk(bld, sp.bytes, 0, decoded)
      }

      result(bld)

    case JsonToken.TStringContinued => decodeStream(reader)
    case other => DecoderUtils.throwUnexpected(reader, "a string", other)
  }

  def decode(str: String): T = {
    val bld = newSizedBuilder(str.length)
    val sp = Scratchpad.get()

    var ofs = 0
    val iterChunks = computeIterChunks(sp)
    val iterChars = iterChunks * encoding.codedChunk

    while (ofs < str.length) {
      val chars = math.min(iterChars, str.length - ofs)
      str.getChars(ofs, ofs + chars, sp.chars, 0)

      if (chars == iterChars) {
        val decoded = encoding.decode(iterChunks, sp.chars, 0, sp.bytes)
        appendChunk(bld, sp.bytes, 0, decoded)
      } else {
        val chunks = chars / encoding.codedChunk

        if (chunks > 0) {
          val decoded = encoding.decode(chunks, sp.chars, 0, sp.bytes)
          appendChunk(bld, sp.bytes, 0, decoded)
        }

        val rr = chunks * encoding.codedChunk
        if (rr < chars) {
          val decoded = encoding.decodeLast(sp.chars, rr, chars - rr, sp.bytes)
          appendChunk(bld, sp.bytes, 0, decoded)
        }
      }

      ofs += chars
    }

    result(bld)
  }
}
