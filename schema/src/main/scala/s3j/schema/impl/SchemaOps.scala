package s3j.schema.impl

import s3j.ast.{JsArray, JsObject, JsValue}
import s3j.format.BasicFormats.given
import s3j.*

object SchemaOps {
  val RefKey = "$ref"

  /** @return JSON object with internal reference */
  def intReference(i: Int): JsObject = JsObject(RefKey -> ("~" + i))

  /**
   * Expand `{"$ref": "~NNN"}` objects using provided `refs`. Refs not conforming to this format are not expanded and
   * are left as-is.
   *
   * @param base JSON document to process
   * @param refs References
   * @return     JSON document with replaced references
   */
  def expandRefs(base: JsValue, refs: Seq[JsValue]): JsValue =
    base match {
      case obj: JsObject if obj.has(RefKey) =>
        val ref = obj(RefKey).convertTo[String]
        if (ref.startsWith("~")) refs(ref.tail.toInt).asObject ++ obj.excludeKey(RefKey)
        else obj

      case obj: JsObject => new JsObject(obj.items.map { case (k, v) => k -> expandRefs(v, refs) }, obj.order)
      case arr: JsArray => new JsArray(arr.value.map(expandRefs(_, refs)))
      case other => other
    }

  /**
   * Overloaded version of `expandRefs`, using strings for input and output.
   *
   * @see [[expandRefs(JsValue, JsValue)]]
   */
  def expandRefs(base: String, refs: Seq[String]): String =
    expandRefs(base.fromJson[JsValue], refs.map(_.fromJson[JsValue])).toJsonString
}
