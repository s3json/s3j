package s3j
import s3j.format.util.DecoderUtils
import s3j.io.{JsonReader, JsonWriter}

/** 'Discarded' value to explicitly drop some fields while decoding. */
sealed trait JsDiscarded

case object JsDiscarded extends JsDiscarded {
  /** Format for [[JsDiscarded]]. Produces `null`s on encoding. */
  given discardedFormat: JsonFormat[JsDiscarded] with {
    def encode(writer: JsonWriter, value: JsDiscarded): Unit = writer.nullValue()
    def decode(reader: JsonReader): JsDiscarded = { DecoderUtils.skipValue(reader); JsDiscarded }
  }
}
