package s3j.format.impl

import s3j.format.JsonFormat
import s3j.io.{JsonWriter, Scratchpad}

import java.io.ByteArrayOutputStream
import scala.collection.mutable

object BinaryFormats {
  class ByteArrayFormat(encoding: BinaryEncoding) extends BinaryFormat[Array[Byte]](encoding) {
    protected type Iterator = IteratorImpl
    protected type Builder = ByteArrayOutputStream

    protected class IteratorImpl(val data: Array[Byte], var pos: Int) {
      val length: Int = data.length
    }

    protected def newBuilder: Builder = new ByteArrayOutputStream()
    protected def newBuilder(expectedSize: Int): ByteArrayOutputStream = new ByteArrayOutputStream(expectedSize)

    protected def appendChunk(b: Builder, chunk: Array[Byte], offset: Int, length: Int): Unit =
      b.write(chunk, offset, length)

    protected def result(b: Builder): Array[Byte] = b.toByteArray

    protected def newIterator(value: Array[Byte]): Iterator = new IteratorImpl(value, 0)
    protected def valueLength(value: Array[Byte]): Int = value.length

    protected def readChunk(it: IteratorImpl, target: Array[Byte], offset: Int, length: Int): Int = {
      val r = math.min(length, it.length - it.pos)
      System.arraycopy(it.data, it.pos, target, offset, length)
      it.pos += r
      r
    }

    protected override def performEncoding(value: Array[Byte], write: (Array[Char], Int, Int) => Unit): Unit = {
      val sp = Scratchpad.get()
      val maxChunks = math.min(value.length / encoding.byteChunk, sp.chars.length / encoding.codedChunk)

      var ofs = 0
      while (ofs < value.length) {
        val chunks = math.min((value.length - ofs) / encoding.byteChunk, maxChunks)
        if (chunks > 0) {
          val len = encoding.encode(chunks, value, ofs, sp.chars)
          write(sp.chars, 0, len)
          ofs += chunks * encoding.byteChunk
        } else {
          val len = encoding.encodeLast(value, ofs, value.length - ofs, sp.chars)
          write(sp.chars, 0, len)
          ofs = value.length
        }
      }
    }

    override def toString: String = s"ByteArrayFormat($encoding)"
  }
}
