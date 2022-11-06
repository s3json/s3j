package s3j.format.util

import s3j.io.JsonWriter

/**
 * JSON writer that suppresses top-level object generation, allowing multiple objects to be merged into one.
 *
 * Recognized by [[ObjectFormatUtils.writeBeginObject]] and [[ObjectFormatUtils.writeEndObject]]: it will incur no
 * overhead at all for anything using these functions to encode objects.
 * 
 * Does not handle insane cases when objects are written as raw chunks
 */
final case class StripObjectWriter(val writer: JsonWriter) extends JsonWriter {
  private var _nesting: Int = 0
  
  def beginArray(): JsonWriter = {
    _nesting += 1
    writer.beginArray()
    this
  }
  
  def beginObject(): JsonWriter = {
    if (_nesting > 0) writer.beginObject()
    _nesting += 1
    this
  }

  def beginString(): JsonWriter = {
    _nesting += 1
    writer.beginString()
    this
  }

  def end(): JsonWriter = {
    _nesting -= 1
    if (_nesting > 0) writer.end()
    this
  }

  def key(key: String): JsonWriter = { writer.key(key); this }
  def boolValue(value: Boolean): JsonWriter = { writer.boolValue(value); this }
  def unsignedLongValue(value: Long): JsonWriter = writer.unsignedLongValue(value)
  def longValue(value: Long): JsonWriter = writer.longValue(value)
  override def unsignedIntValue(value: Int): JsonWriter = writer.unsignedIntValue(value)
  override def intValue(value: Int): JsonWriter = writer.intValue(value)
  override def unsignedShortValue(value: Short): JsonWriter = writer.unsignedShortValue(value)
  override def shortValue(value: Short): JsonWriter = writer.shortValue(value)
  override def unsignedByteValue(value: Byte): JsonWriter = writer.unsignedByteValue(value)
  override def byteValue(value: Byte): JsonWriter = writer.byteValue(value)
  def doubleValue(value: Double): JsonWriter = { writer.doubleValue(value); this }
  def bigintValue(value: BigInt): JsonWriter = { writer.bigintValue(value); this }
  def bigdecValue(value: BigDecimal): JsonWriter = { writer.bigdecValue(value); this }
  def stringValue(value: String): JsonWriter = { writer.stringValue(value); this }
  def stringValue(value: Array[Char], offset: Int, length: Int): JsonWriter = { writer.stringValue(value, offset, length); this }
  def nullValue(): JsonWriter = { writer.nullValue(); this }
  def haveRawChunks: Boolean = writer.haveRawChunks
  def rawChunk(c: Array[Char], ofs: Int, len: Int): JsonWriter = { writer.rawChunk(c, ofs, len); this }
  def finish(): Unit = writer.finish()
  def close(): Unit = writer.close()
}
