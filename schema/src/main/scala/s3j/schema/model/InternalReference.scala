package s3j.schema.model

import s3j.ast.{JsObject, JsValue}
import s3j.schema.model.SchemaDocument.ReferenceKey

object InternalReference {
  val Prefix = "~"

  /** Wrapper to use references as a full-featured schemas */
  given schemaConversion: Conversion[InternalReference, SchemaDocument] =
    ref => SchemaDocument(`$ref` = Some(ref.toString))

  def parse(str: String): Option[InternalReference] =
    if (!str.startsWith(Prefix)) None
    else {
      val parts = str.tail.split(",")
      val idx = parts.head.toInt
      val forceInline = parts.tail.contains("forceInline")
      Some(InternalReference(idx, forceInline))
    }

  /** Extractor to parse strings via simple `match` */
  def unapply(str: String): Option[(Int, Boolean)] =
    parse(str).map(r => (r.index, r.forceInline))
}

/**
 * Model for serialized internal references.
 *
 * @param index Index in `definitions` array corresponding to this reference
 */
case class InternalReference(index: Int, forceInline: Boolean = false) {
  override def toString: String = {
    val r = InternalReference.Prefix + index
    if (forceInline) r + ",forceInline"
    else r
  }
}
