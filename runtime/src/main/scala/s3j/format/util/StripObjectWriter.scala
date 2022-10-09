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
final case class StripObjectWriter(writer: JsonWriter) extends JsonWriter {
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
  def value(value: Boolean): JsonWriter = { writer.value(value); this }
  def value(value: Long): JsonWriter = { writer.value(value); this }
  def value(value: Double): JsonWriter = { writer.value(value); this }
  def value(value: BigInt): JsonWriter = { writer.value(value); this }
  def value(value: BigDecimal): JsonWriter = { writer.value(value); this }
  def value(value: String): JsonWriter = { writer.value(value); this }
  def value(value: Array[Char], offset: Int, length: Int): JsonWriter = { writer.value(value, offset, length); this }
  def nullValue(): JsonWriter = { writer.nullValue(); this }
  def haveRawChunks: Boolean = writer.haveRawChunks
  def rawChunk(c: Array[Char], ofs: Int, len: Int): JsonWriter = { writer.rawChunk(c, ofs, len); this }
  def finish(): Unit = writer.finish()
  def close(): Unit = writer.close()
}
