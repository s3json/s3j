package s3j.macros.schema

import s3j.schema.model.{InternalReference, SchemaDocument}

import scala.quoted.{Expr, Type}

private[schema] object SchemaExprBuilder {
  def build[T](shouldInline: Boolean, defaultValue: Option[UntypedExpr], exampleValues: Option[Seq[UntypedExpr]])
              (f: SchemaExprBuilder => SchemaDocument): SchemaExpr.Inlined[T] =
  {
    var defs = Vector.empty[SchemaExpr[?]]
    val doc = f(new SchemaExprBuilder {
      def reference(schema: SchemaExpr[_]): SchemaDocument = {
        defs :+= schema
        InternalReference(defs.size - 1)
      }
    })

    SchemaExpr.inlineParts(SchemaExpr.Inlined[T](
      document = doc,
      definitions = defs,
      shouldInline = shouldInline,
      defaultValue = defaultValue,
      exampleValues = exampleValues
    )).asInstanceOf[SchemaExpr.Inlined[T]]
  }
}

trait SchemaExprBuilder {
  /** Insert new reference to sub-schema */
  def reference(schema: SchemaExpr[?]): SchemaDocument
}
