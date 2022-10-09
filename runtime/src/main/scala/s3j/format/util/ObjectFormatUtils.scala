package s3j.format.util

import s3j.JsPath
import s3j.ast.{JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue}
import s3j.format.BasicFormats
import s3j.io.{AstJsonReader, JsonReader, JsonToken, JsonWriter, StreamJsonReader}

import scala.annotation.tailrec

object ObjectFormatUtils {
  /**
   * Writes beginning of the object. Returns a writer that you should use for object serialization (but not for
   * [[writeEndObject]] call!). Recognizes a [[StripObjectWriter]] and removes it.
   */
  def writeBeginObject(writer: JsonWriter): JsonWriter =
    writer match {
      case so: StripObjectWriter => so.writer
      case _ =>
        writer.beginObject()
        writer
    }

  /**
   * Writes end of the object. This should be called with initial writer instance, not with one you got from
   * [[writeBeginObject]].
   */
  def writeEndObject(writer: JsonWriter): Unit =
    writer match {
      case _: StripObjectWriter => // skip
      case _ => writer.end()
    }

  /** Throws an exception when missing key is encountered. */
  def throwMissingKey(reader: JsonReader, key: String): Nothing =
    reader.parseError(s"Missing required key '$key'")

  /** Throws an exception when unknown key is encountered */
  def throwUnknownKey(reader: JsonReader, key: String): Nothing =
    reader.parseError(s"Unknown key: '$key'")

  /** Throws an exception when duplicate key is encountered */
  def throwDuplicateKey(reader: JsonReader, key: String): Nothing =
    reader.parseError(s"Duplicated key: '$key'")

  /**
   * Reads beginning of the object. Returns a reader that you should use for object parsing (but not for
   * [[expectEndObject]] call!). Recognizes a [[AddObjectReader]] and removes it.
   */
  def expectBeginObject(reader: JsonReader): JsonReader = reader match {
    case ao: AddObjectReader => ao.reader
    case _ =>
      val t = reader.nextToken()
      if (t != JsonToken.TObjectStart) {
        reader.parseError("Expected start of object, got " + JsonToken.tokenName(t) + " instead")
      }

      reader
  }

  /**
   * Reads end of the object. This should be called with initial reader instance, not with one you got from
   * [[expectBeginObject]].
   */
  def expectEndObject(reader: JsonReader): Unit = reader match {
    case _: AddObjectReader => // skip
    case _ =>
      val t = reader.nextToken()
      if (t != JsonToken.TStructureEnd) {
        reader.parseError("Expected end of object, got " + JsonToken.tokenName(t) + " instead")
      }
  }

  /** Skip remaining fields in the current object */
  def skipRemainingFields(reader: JsonReader): Unit = {
    while (reader.peekToken == JsonToken.TKey) {
      reader.nextToken() // skip key
      DecoderUtils.skipValue(reader)
    }
  }

  /** Fail parsing if object has an remaining fields */
  def failRemainingFields(reader: JsonReader): Unit =
    reader.peekToken match {
      case JsonToken.TStructureEnd => // ok
      case JsonToken.TKey => throwUnknownKey(reader, reader.key.toString)
      case _ => DecoderUtils.throwUnexpected(reader, "object end", reader.nextToken())
    }

  /**
   * Write contents of @restFields into JSON stream.
   */
  def writeRestFields(writer: JsonWriter, value: JsObject): Unit =
    for (k <- value.keysIterator) {
      writer.key(k)
      BasicFormats.jsValueFormat.encode(writer, value(k))
    }

  /**
   * Encode discriminator and return a writer that should be used for subsequent encoding.
   */
  def encodeDiscriminator(writer: JsonWriter, field: String, value: String): JsonWriter = {
    val ww = writeBeginObject(writer)
    ww.key(field).value(value)
    StripObjectWriter(ww)
  }

  private def decodeBufferedDiscriminator(reader: JsonReader, prefix: JsPath, ast: JsObject, keys: Seq[String],
                                          field: String): DiscriminatorResult =
  {
    if (!keys.contains(field)) {
      reader.parseError("no discriminator field '" + field + "' is present")
    }

    val discriminator = ast.items(field) match {
      case JsString(value) => value
      case other => reader.parseError("discriminator field '" + field +
        "' must be a string; got " + other.typeName)
    }

    val subNode = ast.subObject(keys.filterNot(_ == field))

    DiscriminatorResult(new AstJsonReader(subNode, prefix), discriminator)
  }

  /**
   * Decode and remove discriminator field from a stream. Returns the value of discriminator field and reader for
   * further object parsing.
   *
   * @param reader         Reader to use
   * @param field          Name of discriminator field
   * @param maxLength      Maximum length of discriminator field value
   * @param allowBuffering Whether to allow buffer if discriminator field didn't come first in the stream
   * @return Discriminator result or `null` if enum case exception should be thrown
   */
  def decodeDiscriminator(reader: JsonReader, field: String, maxLength: Int,
                          allowBuffering: Boolean): DiscriminatorResult | Null =
    expectBeginObject(reader) match {
      case innerReader: JsonReader.Buffered =>
        val location = innerReader.location.toPath.getOrElse(JsPath.Root)
        val keys = innerReader.remainingKeys // as we have passed 'expectBeginObject', this should be an object.
        decodeBufferedDiscriminator(innerReader, location, innerReader.readEnclosingValue().asObject, keys, field)

      case innerReader => innerReader.nextToken() match {
        case JsonToken.TKey if innerReader.key.stringEquals(field) =>
          val discriminator = DecoderUtils.decodeStringRaw(innerReader, maxLength)
          if (discriminator == null) null
          else DiscriminatorResult(AddObjectReader(innerReader), discriminator)

        case JsonToken.TKey =>
          if (!allowBuffering) {
            innerReader.parseError("discriminator field '" + field + "' must come first")
          }

          val locationPrefix = reader.location.toPath.getOrElse(JsPath.Root)
          val restFields = new RestFieldsBuilder
          restFields.readField(innerReader.key.toString, innerReader)

          while (innerReader.peekToken == JsonToken.TKey) {
            val key = innerReader.key.toString
            innerReader.nextToken() // consume key
            restFields.readField(key, innerReader)
          }

          // N.B. Don't consume TStructureEnd! It will be read later in main decoder code.

          val result = restFields.result()
          decodeBufferedDiscriminator(innerReader, locationPrefix, result, result.order, field)

        case JsonToken.TStructureEnd => throwMissingKey(innerReader, field)
        case other => DecoderUtils.throwUnexpected(innerReader, "a key", other)
      }
    }

  /**
   * Builder for @restFields decoding. Does not track duplicated keys for performance reasons.
   */
  class RestFieldsBuilder {
    private val items = Map.newBuilder[String, JsValue]
    private val order = Vector.newBuilder[String]

    def readField(key: String, reader: JsonReader): Unit = {
      order += key
      items += key -> BasicFormats.jsValueFormat.decode(reader)
    }

    def result(): JsObject = new JsObject(items.result(), order.result())
  }
}
