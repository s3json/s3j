package s3j.format.impl

object BinaryEncoding {
  abstract class GenericBase64 extends BinaryEncoding {
    protected def alphabet: Array[Char]
    protected def padding: Boolean

    def byteChunk: Int = 3
    def codedChunk: Int = 4

    def encode(n: Int, in: Array[Byte], inOffset: Int, out: Array[Char]): Int = {
      val alpha = alphabet
      val dstEnd = n << 2
      var src = inOffset
      var dst = 0

      while (dst < dstEnd) {
        val b1 = in(src) & 0xFF; src += 1
        val b2 = in(src) & 0xFF; src += 1
        val b3 = in(src) & 0xFF; src += 1

        out(dst) = alpha(b1 >> 2); dst += 1
        out(dst) = alpha(((b1 << 4) | (b2 >> 4)) & 0x3F); dst += 1
        out(dst) = alpha(((b2 << 2) | (b3 >> 6)) & 0x3F); dst += 1
        out(dst) = alpha(b3 & 0x3F); dst += 1
      }

      dstEnd
    }

    def encodeLast(in: Array[Byte], inOffset: Int, bytes: Int, out: Array[Char]): Int = {
      var r = 2
      val alpha = alphabet

      val b1 = in(inOffset) & 0xFF
      out(0) = alpha(b1 >> 2)

      if (bytes == 1) {
        out(1) = alpha((b1 << 4) & 0x3F)
        if (padding) out(2) = '='
      } else {
        val b2 = in(inOffset + 1) & 0xFF
        out(1) = alpha(((b1 << 4) | (b2 >> 4)) & 0x3F)
        out(2) = alpha((b2 << 2) & 0x3F)
        r = 3
      }

      if (padding) {
        out(3) = '='
        r = 4
      }

      r
    }

    private def decodeChar(c: Char): Int = {
      if (c >= 'A' && c <= 'Z') c - 'A'
      else if (c >= 'a' && c <= 'z') c - 'a' + 26
      else if (c >= '0' && c <= '9') c - '0' + 52
      else if (c == '+' || c == '-') 62
      else if (c == '/' || c == '_') 63
      else throw new IllegalArgumentException("Invalid base64 character: " + c + " (%04x)".format(c.toInt))
    }

    // noinspection DuplicatedCode
    def decode(n: Int, in: Array[Char], inOffset: Int, out: Array[Byte]): Int = {
      var srcEnd = inOffset + (n - 1) << 2
      var havePadding = false

      var i = 0
      while (i < 4 && in(srcEnd + i) != '=') i += 1
      if (i == 4) srcEnd += 4
      else havePadding = true

      var src = inOffset
      var dst = 0

      while (src < srcEnd) {
        val c1 = decodeChar(in(src)); src += 1
        val c2 = decodeChar(in(src)); src += 1
        val c3 = decodeChar(in(src)); src += 1
        val c4 = decodeChar(in(src)); src += 1

        out(dst) = ((c1 << 2) | (c2 >> 4)).toByte; dst += 1
        out(dst) = ((c2 << 4) | (c3 >> 2)).toByte; dst += 1
        out(dst) = ((c3 << 6) | c4).toByte; dst += 1
      }

      if (havePadding) {
        val c1 = decodeChar(in(src)); src += 1
        val c2 = decodeChar(in(src)); src += 1
        out(dst) = ((c1 << 2) | (c2 >> 4)).toByte; dst += 1

        if (in(src) != '=') {
          val c3 = decodeChar(in(src)); src += 1
          out(dst) = ((c2 << 4) | (c3 >> 2)).toByte; dst += 1
        }
      }

      dst
    }

    def decodeLast(in: Array[Char], inOffset: Int, chars: Int, out: Array[Byte]): Int = {
      val c1 = decodeChar(in(inOffset))
      if (chars < 2) {
        throw new IllegalStateException("too short base64 trailing chunk: must be at least 2 characters")
      }

      val c2 = decodeChar(in(inOffset + 1))
      out(0) = ((c1 << 2) | (c2 >> 4)).toByte;

      if (chars > 2 && in(inOffset + 2) != '=') {
        val c3 = decodeChar(in(inOffset + 2))
        out(1) = ((c2 << 4) | (c3 >> 2)).toByte
        /* return */ 2
      } else /* return */ 1
    }
  }

  /** Base64 [https://www.rfc-editor.org/rfc/rfc4648#section-4] */
  case object Base64 extends GenericBase64 {
    protected val alphabet: Array[Char] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".toCharArray
    protected def padding: Boolean = true
  }

  /** URL-safe base64 [https://www.rfc-editor.org/rfc/rfc4648#section-5] without padding chars */
  case object Base64Url extends GenericBase64 {
    protected val alphabet: Array[Char] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_".toCharArray
    protected def padding: Boolean = false
  }

  abstract class GenericHex extends BinaryEncoding {
    protected def alphabet: Array[Char]

    def byteChunk: Int = 1
    def codedChunk: Int = 2

    def encode(n: Int, in: Array[Byte], inOffset: Int, out: Array[Char]): Int = {
      val alpha = alphabet
      var src = inOffset
      val srcEnd = inOffset + n
      var dst = 0

      while (src < srcEnd) {
        val b = in(src) & 0xFF; src += 1
        out(dst) = alpha(b >> 4); dst += 1
        out(dst) = alpha(b & 15); dst += 1
      }

      dst
    }

    // byteChunk == 1, so there will be no incomplete chunks
    def encodeLast(in: Array[Byte], inOffset: Int, bytes: Int, out: Array[Char]): Int =
      throw new UnsupportedOperationException("GenericHex.encodeLast")

    private def decodeChar(c: Char): Int = {
      if (c >= '0' && c <= '9') c - '0'
      else if (c >= 'a' && c <= 'f') c - 'a' + 10
      else if (c >= 'A' && c <= 'F') c - 'A' + 10
      else throw new IllegalArgumentException("invalid hex char: " + c)
    }

    // noinspection DuplicatedCode
    def decode(n: Int, in: Array[Char], inOffset: Int, out: Array[Byte]): Int = {
      var src = inOffset
      var dst = 0

      while (dst < n) {
        val b1 = decodeChar(in(src)); src += 1
        val b2 = decodeChar(in(src)); src += 1
        out(dst) = ((b1 << 4) | b2).toByte; dst += 1
      }

      dst
    }

    def decodeLast(in: Array[Char], inOffset: Int, chars: Int, out: Array[Byte]): Int =
      throw new IllegalArgumentException("incomplete hexadecimal group")
  }

  /** Hexadecimal (base16) encoding with lowercase output */
  case object HexLowercase extends GenericHex {
    protected val alphabet: Array[Char] = "0123456789abcdef".toCharArray
  }

  /** Hexadecimal (base16) encoding with uppercase output */
  case object HexUppercase extends GenericHex {
    protected val alphabet: Array[Char] = "0123456789ABCDEF".toCharArray
  }
}

trait BinaryEncoding {
  /** @return Byte (unencoded) size of a single chunk */
  def byteChunk: Int

  /** @return String (coded) size of a single chunk */
  def codedChunk: Int

  /**
   * Encode `n` chunks from input array, placing the result into output array. Returns encoded length.
   * Input array will be `n * byteChunk` bytes long, output array will be `n * codedChunk` characters long.
   */
  def encode(n: Int, in: Array[Byte], inOffset: Int, out: Array[Char]): Int

  /**
   * Encode last incomplete chunk with length of [[bytes]], returning encoded length.
   */
  def encodeLast(in: Array[Byte], inOffset: Int, bytes: Int, out: Array[Char]): Int

  /**
   * Decode `n` chunks from input array, placing the result into output array. Returns decoded length.
   * Input array will be `n * codedChunk` characters long, output array will be `n * byteChunk` bytes long.
   */
  def decode(n: Int, in: Array[Char], inOffset: Int, out: Array[Byte]): Int

  /**
   * Decode last chunk with length of [[chars]], returning decoded length.
   */
  def decodeLast(in: Array[Char], inOffset: Int, chars: Int, out: Array[Byte]): Int
}
