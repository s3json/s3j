package s3j.macros.schema

import s3j.macros.schema.impl.{SchemaInliner, SchemaParser, SchemaSerializer}
import s3j.schema.JsonSchema
import s3j.schema.model.SchemaDocument

import scala.annotation.unused
import scala.quoted.{Expr, Quotes, Type}

object SchemaExpr {
  /** Runtime schema expression */
  case class Runtime[T](expr: Expr[JsonSchema[T]])(using val valueType: Type[T]) extends SchemaExpr[T] {
    def canInline: Boolean = false
  }

  /**
   * Inlined schema expression, a quoted counterpart of [[JsonSchema]].
   *
   * @param document      Schema document
   * @param definitions   Definitions
   * @param shouldInline  Whether this schema should be inlined at runtime (does not affect compile-time inlining)
   * @param defaultValue  Default value for type described by this schema
   * @param exampleValues Example values for type described by this schema
   */
  case class Inlined[T](
    document:       SchemaDocument,
    definitions:    Seq[SchemaExpr[?]] = Nil,
    shouldInline:   Boolean = false,
    defaultValue:   Option[Expr[T]] = None,
    exampleValues:  Option[Seq[Expr[T]]] = None
  )(using val valueType: Type[T]) extends SchemaExpr[T] {
    /** Whether this expression could be inlined */
    def canInline: Boolean =
      // runtime values prevent schema from being computed statically, and thus from being inlined
      shouldInline && defaultValue.isEmpty && exampleValues.isEmpty
  }

  /** Build new inlined schema expression, automatically constructing definitions array */
  def build[T](
    shouldInline:   Boolean               = false, 
    defaultValue:   Option[Expr[T]]       = None,
    exampleValues:  Option[Seq[Expr[T]]]  = None
  )(f: SchemaExprBuilder => SchemaDocument)(using Type[T]): SchemaExpr.Inlined[T] =
    SchemaExprBuilder.build(shouldInline, defaultValue, exampleValues)(f)
  
  /** @return Parsed schema for expr */
  def fromExpr[T](expr: Expr[JsonSchema[T]])(using Quotes, Type[T]): SchemaExpr[T] =
    SchemaParser.fromExpr(expr)

  /** @return Schema serialized as a runtime expression */
  def toExpr[T](schema: SchemaExpr[T], inlineTypes: Boolean = true)(using Quotes): Expr[JsonSchema[T]] =
    SchemaSerializer.toExpr(schema, !inlineTypes)

  /** @return Type of serialized schema instance, possibly with extra inlining information */
  def toType[T, R <: JsonSchema[T]](schema: SchemaExpr[T])(using Quotes): Type[R] =
    SchemaSerializer.toType(schema)

  /** @return Schema with all possible dependencies inlined */
  def inlineParts[T](schema: SchemaExpr[T]): SchemaExpr[T] =
    SchemaInliner.inlineParts(schema)

  /** @return Schema expression for internal references */
  def internalReference[T](index: Int)(using Type[T]): SchemaExpr.Inlined[T] =
    SchemaExpr.Inlined(SchemaDocument.referenceSchema(index))
}

/**
 * Schema expression representation, either as inline schema or as runtime object.
 */
sealed abstract class SchemaExpr[T] {
  type Underlying = T
  given valueType: Type[T]

  /** Whether this expression could be inlined */
  def canInline: Boolean
}
