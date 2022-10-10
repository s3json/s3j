package s3j.format.impl

import s3j.format.{JsonFormat, StringyFormat}
import s3j.format.util.DecoderUtils
import s3j.io.{JsonReader, JsonToken, JsonWriter}

import scala.collection.mutable

object NumberFormats {
  /** @return Number from JSON stream, presented as string, or `null` on length exhaustion */
  def readNumberStringRaw(reader: JsonReader, maxLength: Int): String | Null =
    reader.nextToken() match {
      case JsonToken.TNumber =>
        if (reader.chunk.remaining > maxLength) null
        else reader.chunk.toString

      case JsonToken.TNumberContinued =>
        val sb = new mutable.StringBuilder()
        reader.chunk.appendTo(sb)

        var token = JsonToken.TNumberContinued
        while (token == JsonToken.TNumberContinued) {
          token = reader.nextToken()
          reader.chunk.appendTo(sb)

          if (sb.length > maxLength) return null
        }

        sb.result()

      case other => DecoderUtils.throwUnexpected(reader, "a number", other)
    }

  /** @return Number from JSON stream, presented as string */
  def readNumberString(reader: JsonReader, maxLength: Int, errorMessage: String): String = {
    val r = readNumberStringRaw(reader, maxLength)
    if (r == null) reader.parseError(errorMessage)
    else r
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

  /** Stringy signed byte format */
  given byteStringyFormat: StringyFormat[Byte] with {
    def encode(value: Byte): String = value.toString
    def decode(str: String): Byte = java.lang.Byte.parseByte(str, 10)
    override def toString: String = "stringyByteFormat"
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

  /** Stringy signed short format */
  given shortStringyFormat: StringyFormat[Short] with {
    def encode(value: Short): String = value.toString
    def decode(str: String): Short = java.lang.Short.parseShort(str, 10)
    override def toString: String = "stringyShortFormat"
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

  /** Stringy signed int format */
  given intStringyFormat: StringyFormat[Int] with {
    def encode(value: Int): String = value.toString
    def decode(str: String): Int = java.lang.Integer.parseInt(str, 10)
    override def toString: String = "stringyIntFormat"
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

  /** Stringy long format */
  given longStringyFormat: StringyFormat[Long] with {
    def encode(value: Long): String = value.toString
    def decode(str: String): Long = java.lang.Long.parseLong(str, 10)
    override def toString: String = "stringyLongFormat"
  }

  /** Single-precision float format */
  given floatFormat: JsonFormat[Float] with {
    def encode(writer: JsonWriter, value: Float): Unit = writer.doubleValue(value)
    def decode(reader: JsonReader): Float = decodeDouble(reader).toFloat
    override def toString: String = "floatFormat"
  }

  /** Stringy single-precision float format */
  given stringyFloatFormat: StringyFormat[Float] with {
    def encode(value: Float): String = value.toString
    def decode(str: String): Float = java.lang.Float.parseFloat(str)
    override def toString: String = "stringyFloatFormat"
  }

  /** Double-precision float format */
  given doubleFormat: JsonFormat[Double] with {
    def encode(writer: JsonWriter, value: Double): Unit = writer.doubleValue(value)
    def decode(reader: JsonReader): Double = decodeDouble(reader)
    override def toString: String = "doubleFormat"
  }

  /** Stringy double-precision float format */
  given stringyDoubleFloat: StringyFormat[Double] with {
    def encode(value: Double): String = value.toString
    def decode(str: String): Double = java.lang.Double.parseDouble(str)
    override def toString: String = "stringyDoubleFormat"
  }

  /** @return [[JsonFormat]] instance for [[BigInt]]s with number length limit */
  def bigIntFormat(maxLength: Int): JsonFormat[BigInt] =
    new JsonFormat[BigInt] {
      def encode(writer: JsonWriter, value: BigInt): Unit = writer.bigintValue(value)
      def decode(reader: JsonReader): BigInt = {
        val s = readNumberStringRaw(reader, maxLength)
        if (s == null) reader.parseError("number is too long, maximum allowed " + maxLength + " digits")
        else BigInt(s)
      }

      override def toString: String = s"BigIntFormat(maxLength=$maxLength)"
    }

  /** @return [[JsonFormat]] instance for [[BigDecimal]]s with number length and scale limit */
  def bigDecFormat(maxLength: Int, maxScale: Int): JsonFormat[BigDecimal] =
    new JsonFormat[BigDecimal] {
      def encode(writer: JsonWriter, value: BigDecimal): Unit = writer.bigdecValue(value)
      def decode(reader: JsonReader): BigDecimal = {
        val s = readNumberStringRaw(reader, maxLength)
        if (s == null) reader.parseError("number is too long, maximum allowed " + maxLength + " digits")
        else {
          val d = BigDecimal(s)
          if (math.abs(d.scale) > maxScale) reader.parseError("number scale is out of range: maximum is " + maxScale)
          else d
        }
      }

      override def toString: String = s"BigDecFormat(maxLength=$maxLength, maxScale=$maxScale)"
    }

  /** Default [[BigInt]] format instance with maximum length of 1024 digits */
  given bigIntFormat: JsonFormat[BigInt] = bigIntFormat(1024)

  /** Default [[BigDecimal]] format instance with maximum length of 1024 digits and maximum scale of 8192 */
  given bigDecFormat: JsonFormat[BigDecimal] = bigDecFormat(1024, 8192)
}
