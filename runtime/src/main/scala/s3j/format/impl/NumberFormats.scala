package s3j.format.impl

import s3j.format.JsonFormat
import s3j.format.util.DecoderUtils
import s3j.io.{JsonReader, JsonToken, JsonWriter}

import scala.collection.mutable

object NumberFormats {
  /** @return Number from JSON stream, presented as string */
  def readNumberString(reader: JsonReader, maxLength: Int, errorMessage: String): String =
    reader.nextToken() match {
      case JsonToken.TNumber =>
        if (reader.chunk.remaining > maxLength) reader.parseError(errorMessage)
        reader.chunk.toString

      case JsonToken.TNumberContinued =>
        val sb = new mutable.StringBuilder()
        reader.chunk.appendTo(sb)

        var token = JsonToken.TNumberContinued
        while (token == JsonToken.TNumberContinued) {
          token = reader.nextToken()
          reader.chunk.appendTo(sb)

          if (sb.length > maxLength) reader.parseError(errorMessage)
        }

        sb.result()

      case other => DecoderUtils.throwUnexpected(reader, "a number", other)
    }

  private def parseLong(reader: JsonReader, s: String, unsigned: Boolean): Long =
    try {
      if (unsigned) java.lang.Long.parseUnsignedLong(s, 10)
      else java.lang.Long.parseLong(s, 10)
    } catch {
      case e: NumberFormatException => reader.parseError(e.getMessage)
    }

  /**
   * Decode long number from JSON stream.
   *
   * @param reader   JSON reader to use
   * @param unsigned Whether to interpret
   * @return
   */
  def decodeLong(reader: JsonReader, unsigned: Boolean): Long =
    reader.peekToken match {
      case JsonToken.TNumber =>
        reader.nextToken()
        parseLong(reader, reader.chunk.toString, unsigned)

      case JsonToken.TNumberContinued =>
        val str = readNumberString(reader, 19, "too long number")
        parseLong(reader, str, unsigned)

      case other => DecoderUtils.throwUnexpected(reader, "a number", other)
    }

  /**
   * Decode double precision number from JSON stream.
   */
  def decodeDouble(reader: JsonReader): Double =
    reader.peekToken match {
      case JsonToken.TNumber =>
        reader.nextToken()
        try java.lang.Double.parseDouble(reader.chunk.toString)
        catch { case e: NumberFormatException => reader.parseError(e.getMessage) }

      case JsonToken.TNumberContinued =>
        try java.lang.Double.parseDouble(readNumberString(reader, 42, "too long double"))
        catch { case e: NumberFormatException => reader.parseError(e.getMessage) }

      case JsonToken.TString | JsonToken.TStringContinued =>
        val str = DecoderUtils.decodeString(reader, 16, "too long string in number position").toLowerCase
        if (str == "nan") Double.NaN
        else if (str == "infinity" || str == "inf") Double.PositiveInfinity
        else if (str == "-infinity" || str == "-inf") Double.NegativeInfinity
        else reader.parseError("unexpected string '" + str + "', expected a number")

      case other => DecoderUtils.throwUnexpected(reader, "a number", other)
    }

  /** Signed byte format */
  given byteFormat: JsonFormat[Byte] with {
    def encode(writer: JsonWriter, value: Byte): Unit = writer.longValue(value)
    def decode(reader: JsonReader): Byte = {
      val r = decodeLong(reader, unsigned = false)
      if (r < Byte.MinValue || r > Byte.MaxValue) reader.parseError("value is out of range for byte: " + r)
      r.toByte
    }

    override def toString: String = "byteFormat"
  }

  /** Unsigned byte format */
  val unsignedByteFormat: JsonFormat[Byte] = new JsonFormat[Byte] {
    def encode(writer: JsonWriter, value: Byte): Unit = writer.longValue(value & 0xFF)
    def decode(reader: JsonReader): Byte = {
      val r = decodeLong(reader, unsigned = false)
      if (r < 0 || r > 255) reader.parseError("value is out of range for unsigned byte: " + r)
      r.toByte
    }

    override def toString: String = "unsignedByteFormat"
  }

  /** Signed short format */
  given shortFormat: JsonFormat[Short] with {
    def encode(writer: JsonWriter, value: Short): Unit = writer.longValue(value)
    def decode(reader: JsonReader): Short = {
      val r = decodeLong(reader, unsigned = false)
      if (r < Short.MinValue || r > Short.MaxValue) reader.parseError("value is out of range for short: " + r)
      r.toShort
    }

    override def toString: String = "shortFormat"
  }

  /** Unsigned short format */
  val unsignedShortFormat: JsonFormat[Short] = new JsonFormat[Short] {
    def encode(writer: JsonWriter, value: Short): Unit = writer.longValue(value & 0xFFFF)
    def decode(reader: JsonReader): Short = {
      val r = decodeLong(reader, unsigned = false)
      if (r < 0 || r > 0xFFFF) reader.parseError("value is out of range for unsigned short: " + r)
      r.toShort
    }

    override def toString: String = "unsignedShortFormat"
  }

  /** Signed int format */
  given intFormat: JsonFormat[Int] with {
    def encode(writer: JsonWriter, value: Int): Unit = writer.longValue(value)
    def decode(reader: JsonReader): Int = {
      val r = decodeLong(reader, unsigned = false)
      if (r < Int.MinValue || r > Int.MaxValue) reader.parseError("value is out of range for int: " + r)
      r.toInt
    }

    override def toString: String = "intFormat"
  }

  /** Unsigned int format */
  val unsignedIntFormat: JsonFormat[Int] = new JsonFormat[Int] {
    def encode(writer: JsonWriter, value: Int): Unit = writer.longValue(value & 0xFFFFFFFFL)
    def decode(reader: JsonReader): Int = {
      val r = decodeLong(reader, unsigned = false)
      if (r < 0 || r > 0xFFFFFFFFL) reader.parseError("value is out of range for unsigned int: " + r)
      r.toInt
    }

    override def toString: String = "unsignedIntFormat"
  }

  /** Signed long format */
  given longFormat: JsonFormat[Long] with {
    def encode(writer: JsonWriter, value: Long): Unit = writer.longValue(value)
    def decode(reader: JsonReader): Long = decodeLong(reader, unsigned = false)
    override def toString: String = "longFormat"
  }

  /** Unsigned long format */
  val unsignedLongFormat: JsonFormat[Long] = new JsonFormat[Long] {
    def encode(writer: JsonWriter, value: Long): Unit = writer.longValue(value, unsigned = true)
    def decode(reader: JsonReader): Long = decodeLong(reader, unsigned = true)
    override def toString: String = "unsignedLongFormat"
  }

  /** Single-precision float format */
  given floatFormat: JsonFormat[Float] with {
    def encode(writer: JsonWriter, value: Float): Unit = writer.doubleValue(value)
    def decode(reader: JsonReader): Float = decodeDouble(reader).toFloat
    override def toString: String = "floatFormat"
  }

  /** Double-precision float format */
  given doubleFormat: JsonFormat[Double] with {
    def encode(writer: JsonWriter, value: Double): Unit = writer.doubleValue(value)
    def decode(reader: JsonReader): Double = decodeDouble(reader)
    override def toString: String = "doubleFormat"
  }

  // TODO: BigInt
  // TODO: BigDec
}
