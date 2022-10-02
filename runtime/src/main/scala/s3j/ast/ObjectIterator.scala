package s3j.ast

private[ast] class ObjectIterator(obj: JsObject) extends Iterator[(String, JsValue)] {
  private val keys: Iterator[String] = if (obj.order.isEmpty) obj.items.keysIterator else obj.order.iterator

  def hasNext: Boolean = keys.hasNext
  
  def next(): (String, JsValue) = {
    val k = keys.next()
    k -> obj.items(k)
  }
}
