package s3j.io

abstract class JsonWriter {
  /** Write array header and enter array mode */
  def beginArray(): JsonWriter

  /** Write object header and enter object mode */
  def beginObject(): JsonWriter

  /** Write string header and enter string mode - [[rawChunk]] will now write escaped data */
  def beginString(): JsonWriter

  /** Write trailer for current mode and go one level up */
  def end(): JsonWriter

  /** Write object key to stream. */
  def key(key: String): JsonWriter

  /** Write boolean value to stream */
  def boolValue(value: Boolean): JsonWriter

  /** Write byte value to stream */
  def byteValue(value: Byte): JsonWriter = longValue(value)

  /** Write unsigned byte value to stream */
  def unsignedByteValue(value: Byte): JsonWriter = longValue(value & 0xFF)

  /** Write short value to stream */
  def shortValue(value: Short): JsonWriter = longValue(value)

  /** Write unsigned short value to stream */
  def unsignedShortValue(value: Short): JsonWriter = longValue(value & 0xFFFF)

  /** Write int value to stream */
  def intValue(value: Int): JsonWriter = longValue(value)

  /** Write unsigned int value to stream */
  def unsignedIntValue(value: Int): JsonWriter = longValue(value & 0xFFFFFFFFL)

  /** Write long value to stream */
  def longValue(value: Long): JsonWriter

  /** Write unsigned long value to stream */
  def unsignedLongValue(value: Long): JsonWriter

  /** Write float value to stream */
  def floatValue(value: Float): JsonWriter = doubleValue(value)

  /** Write double value to stream */
  def doubleValue(value: Double): JsonWriter

  /** Write [[BigInt]] value to stream */
  def bigintValue(value: BigInt): JsonWriter

  /** Write [[BigDecimal]] value to stream */
  def bigdecValue(value: BigDecimal): JsonWriter

  /** Write string value (or chunk when in string mode) to stream */
  def stringValue(value: String): JsonWriter

  /** Write string value (or chunk when in string mode) to stream. */
  def stringValue(value: Array[Char], offset: Int, length: Int): JsonWriter

  /** Write null value to stream */
  def nullValue(): JsonWriter

  /** Whether this writer is capable of writing arbitrary data chunks */
  def haveRawChunks: Boolean

  /** @see dataChunk(Array[Char], Int, Int) */
  def rawChunk(chunk: Array[Char]): JsonWriter = rawChunk(chunk, 0, chunk.length)

  /**
   * Put raw chunk of data into JSON stream. This method serves as an escape hatch when you have more efficient way to
   * serialize something (e.g. numbers) rather than casting to some built-in type and then serializing that. Invalid
   * JSON could be produced when this method is misused.
   *
   * Not all JSON writers are backed by character stream of some kind. Use [[haveRawChunks]] to check support prior to
   * invoking this method.
   */
  def rawChunk(chunk: Array[Char], offset: Int, length: Int): JsonWriter

  /** Finish writing (checking for correctness of the output) and close this writer */
  def finish(): Unit

  /** Forcibly close this writer, releasing all its resources. */
  def close(): Unit
}
