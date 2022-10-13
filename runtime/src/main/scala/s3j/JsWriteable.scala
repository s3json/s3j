package s3j

import s3j.ast.JsValue
import s3j.format.impl.JsValueFormat
import s3j.io.{AstJsonWriter, JsonReader, JsonWriter, StreamJsonWriter}

import java.io.StringWriter

object JsWriteable {
  /** Convert anything into [[JsWriteable]] given implicit encoder */
  given jsWriteableEncoder[T](using encoder: JsonEncoder[T]): Conversion[T, JsWriteable] =
    value => writer => encoder.encode(writer, value)

  /** Convert JsValue into [[JsWriteable]], which is a bit of special case */
  given jsWriteableValue: Conversion[JsValue, JsWriteable] = JsValueWritable(_)

  /** Format to embed [[JsWriteable]]s in other structures. Decodes as a [[JsValueWritable]]. */
  given jsonWriteableFormat: JsonFormat[JsWriteable] with {
    def encode(writer: JsonWriter, value: JsWriteable): Unit = value.encode(writer)
    def decode(reader: JsonReader): JsWriteable = JsValueWritable(JsValueFormat.decode(reader))
  }

  /** Special case to avoid re-creating tree when we already have one */
  case class JsValueWritable(value: JsValue) extends JsWriteable {
    def encode(writer: JsonWriter): Unit = JsValueFormat.encode(writer, value)
    override def toJsonValue: JsValue = value
  }
}

/** Helper class when specific type is not so important */
abstract class JsWriteable {
  /** Encode stored value into given writer */
  def encode(writer: JsonWriter): Unit

  /** @return Stored value encoded as string */
  def toJsonString: String = toJsonString(0)

  /** @return Stored value encoded as string with given indentation */
  def toJsonString(indent: Int): String = {
    val stringWriter = new StringWriter()
    val jsonWriter = new StreamJsonWriter(stringWriter, indent)
    encode(jsonWriter)
    jsonWriter.close()
    stringWriter.toString
  }

  /** @return Stored value encoded as JSON tree */
  def toJsonValue: JsValue = {
    val jsonWriter = new AstJsonWriter
    encode(jsonWriter)
    jsonWriter.result()
  }

  override def toString: String = toJsonString
}
