package s3j.io

import s3j.ast.JsValue
import s3j.format.{JsonDecoder, JsonEncoder}

import java.io.{Reader, StringReader, StringWriter}

object IoExtensions {
  extension [T](obj: T)(using enc: JsonEncoder[_ >: T]) {
    /** @return Object serialized as JsValue */
    def toJsonValue: JsValue = {
      val writer = new AstJsonWriter
      enc.encode(writer, obj)
      writer.result()
    }

    /** @return Object serialized as compact JSON string */
    def toJsonString: String = toJsonString(0)

    /** @return Object serialized as JSON string */
    def toJsonString(indent: Int): String = {
      val writer = new StringWriter()
      val jsonWriter = new StreamJsonWriter(writer, indent)
      enc.encode(jsonWriter, obj)
      writer.toString
    }
  }
  
  extension (str: String) {
    def convertTo[T](using dec: JsonDecoder[_ <: T]): T = dec.decode(new StreamJsonReader(new StringReader(str)))
  }
  
  extension (reader: Reader) {
    def convertTo[T](using dec: JsonDecoder[_ <: T]): T = dec.decode(new StreamJsonReader(reader))
  }
}
