package s3j.ast

// '$' is used to avoid confusion with actual JSON contents when using common names like 'x', 'base', 'data' etc
class JsDynamic(val $: JsValue) extends Selectable {
  // TODO: ParseException's for errors

  // TODO: def as[T]

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
