package s3j.format

import s3j.ast.JsValue
import s3j.format.impl.{JsValueFormat, StringFormat}
import s3j.io.{JsonReader, JsonToken, JsonWriter}

object BasicFormats {
  given jsValueFormat: JsonFormat[JsValue] = new JsValueFormat
  given stringFormat: JsonFormat[String] = new StringFormat(Int.MaxValue)
}
