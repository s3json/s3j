package s3j.macros.schema.impl

import s3j.ast.{JsArray, JsObject, JsValue}
import s3j.macros.schema.SchemaExpr
import s3j.schema.impl.SchemaOps
import s3j.{*, given}
import s3j.schema.model.{InternalReference, SchemaDocument}
import s3j.schema.model.SchemaDocument.ReferenceKey

private[schema] object SchemaInliner {
  private class InliningContext {
    var defsCache: Map[SchemaExpr[?], Int] = Map.empty
    var defs: Vector[SchemaExpr[?]] = Vector.empty

    def addDefinition(sch: SchemaExpr[?]): Int = {
      if (defsCache.contains(sch)) {
        return defsCache(sch)
      }

      val idx = defs.size
      defs :+= sch
      defsCache += sch -> idx
      idx
    }
  }

  private def inlineInner[T](context: InliningContext, sch: SchemaExpr.Inlined[T]): JsObject = {
    def processRef(idx: Int, force: Boolean, v: JsObject): JsObject = {
      val defn = sch.definitions(idx)

      if (!defn.canInline || !(defn.shouldInline || force)) {
        val newIdx = context.addDefinition(defn)
        v.replaceValue(ReferenceKey, InternalReference(newIdx).toString)
      } else inlineInner(context, defn.asInstanceOf[SchemaExpr.Inlined[?]]) ++ v.excludeKey(ReferenceKey)
    }

    def processJson(v: JsValue): JsValue = v match {
      case v: JsObject if v.has(ReferenceKey) =>
        v(ReferenceKey).convertTo[String] match {
          case InternalReference(idx, forceInline) =>
            processRef(idx, forceInline, v)

          case _ => v
        }

      case v: JsObject => new JsObject(v.items.map((k, v) => k -> processJson(v)), v.order)
      case v: JsArray => new JsArray(v.value.map(processJson))
      case v => v
    }

    processJson(sch.document.toJsonValue).asObject
  }

  private def doInline[T](sch: SchemaExpr.Inlined[T]): SchemaExpr.Inlined[T] = {
    val ctx = new InliningContext
    val doc = inlineInner(ctx, sch).convertTo[SchemaDocument]

    sch.copy(
      document = doc,
      definitions = ctx.defs
    )
  }

  /**
   * Inline everything that could be statically inlined.
   *
   * @param sch Schema expression
   * @tparam T  Type described by schema
   * @return    Schema expression with inlined parts
   */
  def inlineParts[T](sch: SchemaExpr[T]): SchemaExpr[T] =
    sch match {
      case _: SchemaExpr.Runtime[T] => sch
      case inl: SchemaExpr.Inlined[T] => doInline(inl)
    }
}
