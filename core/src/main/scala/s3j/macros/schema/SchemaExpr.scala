package s3j.macros.schema

import s3j.macros.modifiers.ModifierSet
import s3j.macros.schema.impl.{SchemaInliner, SchemaParser, SchemaSerializer, SchemaTransformer}
import s3j.schema.JsonSchema
import s3j.schema.model.{InternalReference, SchemaDocument}

import scala.annotation.unused
import scala.quoted.{Expr, Quotes, Type}

object SchemaExpr {
  /** Runtime schema expression */
  case class Runtime[T](expr: Expr[JsonSchema[T]]) extends SchemaExpr[T] {
    def shouldInline: Boolean = false
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
    defaultValue:   Option[UntypedExpr] = None,
    exampleValues:  Option[Seq[UntypedExpr]] = None
  ) extends SchemaExpr[T] {
    /** Whether this expression could be inlined */
    def canInline: Boolean =
      // runtime values prevent schema from being computed statically, and thus from being inlined
      defaultValue.isEmpty && exampleValues.isEmpty
  }

  /** Build new inlined schema expression, automatically constructing definitions array */
  def build[T](
    shouldInline:   Boolean                   = false, 
    defaultValue:   Option[UntypedExpr]       = None,
    exampleValues:  Option[Seq[UntypedExpr]]  = None
  )(f: SchemaExprBuilder => SchemaDocument): SchemaExpr.Inlined[T] =
    SchemaExprBuilder.build(shouldInline, defaultValue, exampleValues)(f)
  
  /** @return Parsed schema for expr */
  def fromExpr[T](expr: Expr[JsonSchema[T]])(using Quotes, Type[T]): SchemaExpr[T] =
    SchemaParser.fromExpr(expr)

  /** @return Parsed schema for untyped expression */
  def fromUntypedExpr(expr: Expr[JsonSchema[?]])(using Quotes): SchemaExpr[?] =
    SchemaParser.fromUntypedExpr(expr)

  /** @return Schema serialized as a runtime expression */
  def toExpr[T](schema: SchemaExpr[T], inlineTypes: Boolean = true)(using Quotes, Type[T]): Expr[JsonSchema[T]] =
    SchemaSerializer.toExpr(schema, !inlineTypes)

  /** @return Type of serialized schema instance, possibly with extra inlining information */
  def toType[T, R <: JsonSchema[T]](schema: SchemaExpr[T])(using Quotes, Type[T]): Type[R] =
    SchemaSerializer.toType(schema)

  /** @return Schema with all possible dependencies inlined */
  def inlineParts[T](schema: SchemaExpr[T]): SchemaExpr[T] =
    SchemaInliner.inlineParts(schema)

  /** @return Schema with specific fields added (overwriting anything that was there in source schema) */
  def augment[T](schema: SchemaExpr[T], overrides: SchemaDocument): SchemaExpr[T] = 
    inlineParts(SchemaExpr.Inlined(
      document = overrides.copy(`$ref` = Some(InternalReference(0).toString)),
      definitions = Seq(schema),
      shouldInline = true
    ))

  /** @return Schema augmented with content from schema modifiers */
  def augmentModifiers[T](schema: SchemaExpr[T], modifiers: ModifierSet)(using Quotes): SchemaExpr[T] =
    SchemaTransformer.augmentModifiers(schema, modifiers)
    
  /** @return `root` schemas with applied overlay. */
  def overlay[T](root: SchemaExpr[T], overlay: SchemaExpr.Inlined[T]): SchemaExpr[T] =
    SchemaTransformer.overlay(root, overlay)
    
  /** Schema adjusted to make field optional */
  def makeOptional[T](schema: SchemaExpr[T]): SchemaExpr[Option[T]] =
    SchemaTransformer.makeOptional(schema)
}

/**
 * Schema expression representation, either as inline schema or as runtime object.
 */
sealed abstract class SchemaExpr[T] {
  /** Whether this expression could be inlined */
  def canInline: Boolean
  
  /** Whether this expression should be inlined */
  def shouldInline: Boolean
  
  /** @return This schema typed as [[SchemaExpr.Inlined]] if possible. */
  def asInlined: SchemaExpr.Inlined[T] = this match {
    case inl: SchemaExpr.Inlined[T] => inl
    case _ => throw new IllegalStateException("Schema " + this + " is not an inlined schema")
  }
  
  /** @return This schema with erased type `T` */
  def asErased: SchemaExpr[Any] = this.asInstanceOf[SchemaExpr[Any]]
}
