package s3j.ast

import s3j.format.JsonDecoder
import s3j.io.AstJsonReader

// '$' is used to avoid confusion with selectDynamic when picking JSON contents
class JsDynamic(val $: JsValue) extends Selectable {
  // TODO: ParseException's for errors

  def convertTo[T](using d: JsonDecoder[T]): T = d.decode(new AstJsonReader($))
  def as[T](using d: JsonDecoder[T]): T = d.decode(new AstJsonReader($))

  def selectDynamic(name: String): JsDynamic =
    $ match {
      case obj: JsObject => new JsDynamic(obj.items(name))
      case _ => throw new RuntimeException(s"Attempting to get key '$name' of non-object")
    }

  def apply(i: Int): JsDynamic =
    $ match {
      case arr: JsArray => new JsDynamic(arr.value(i))
      case _ => throw new RuntimeException(s"Attempting to take index subscript of non-array")
    }
}
