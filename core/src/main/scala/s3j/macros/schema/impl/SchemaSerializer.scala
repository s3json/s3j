package s3j.macros.schema.impl

import s3j.macros.schema.{SchemaExpr, UntypedExpr}
import s3j.schema.{InlineSchema, JsonSchema}
import s3j.*
import s3j.ast.JsValue

import scala.quoted.{Expr, Quotes, Type, Varargs, quotes}

private[schema] object SchemaSerializer {
  def generateCode[T](inl: SchemaExpr.Inlined[T])(using Quotes, Type[T]): Expr[JsonSchema[T]] = {
    def definitions(using q: Quotes): Expr[JsonSchema.DefinitionsFn] = {
      import q.reflect.*
      val exprs = inl.definitions.map(e => toExpr(e.asErased, noInline = true))
      if (exprs.nonEmpty) '{ () => JsonSchema.definitions( ${Varargs[JsonSchema[?]](exprs) }: _*)}
      else '{ JsonSchema.noDefinitions }
    }

    def summonEncoder[T](using q: Quotes, tt: Type[T]): Expr[JsonEncoder[T]] = {
      import q.reflect.*
      Implicits.search(TypeRepr.of[JsonEncoder[T]]) match {
        case s: ImplicitSearchSuccess => s.tree.asExprOf[JsonEncoder[T]]
        case f: ImplicitSearchFailure =>
          report.errorAndAbort("failed to summon encoder for " + Type.show[T])
      }
    }

    def encodeUntyped(v: UntypedExpr): Expr[JsValue] = {
      given Type[v.T] = v.exprType
      '{ ${ summonEncoder[v.T] }.encodeValue(${v.expr}) }
    }

    def defaultValue(using Quotes): Expr[Option[() => JsValue]] =
      inl.defaultValue match {
        case Some(v) => '{ Some(() => ${ encodeUntyped(v) }) }
        case None => '{ None }
      }

    def exampleValues(using Quotes): Expr[Option[() => Seq[JsValue]]] =
      inl.exampleValues.filter(_.nonEmpty) match {
        case Some(vs) => '{ Some(() => Vector(${ Varargs(vs.map(v => encodeUntyped(v))) }:_*)) }
        case None => '{ None }
      }

    '{
      new JsonSchema[T](
        ${ Expr(inl.document.toJsonString) },
        $definitions,
        ${ Expr(inl.shouldInline) },
        $defaultValue,
        $exampleValues
      )
    }
  }

  def toType[T, R <: JsonSchema[T]](schema: SchemaExpr[T])(using q: Quotes, tt: Type[T]): Type[R] = {
    import q.reflect.*
    if (!schema.canInline || !schema.shouldInline) {
      return Type.of[JsonSchema[T]].asInstanceOf[Type[R]]
    }

    val inl: SchemaExpr.Inlined[T] = schema.asInstanceOf[SchemaExpr.Inlined[T]]
    val jsonType = ConstantType(StringConstant(inl.document.toJsonString))
    val depsType =
      if (inl.definitions.isEmpty) TypeRepr.of[Unit]
      else {
        val deps = inl.definitions.map { e => TypeRepr.of(using toType(e.asErased)) }

        if (deps.size == 1) deps.head
        else AppliedType(defn.TupleClass(deps.size).typeRef, deps.toList)
      }

    AppliedType(
      // N.B.: not TypeRepr.of[InlineSchema] - this will dealias it instantly!
      Symbol.requiredModule("s3j.schema.InlineSchema$package").typeMember("InlineSchema").typeRef,
      List(TypeRepr.of[T], jsonType, depsType)
    ).asType.asInstanceOf[Type[R]]
  }

  /**
   * Convert SchemaExpr into it's runtime representation.
   *
   * @param schema    Schema to convert
   * @param noInline  Suppress generation of [[InlineSchema]] type ascriptions when it's known that they will never
   *                  be parsed.
   * @return          Tree with runtime [[JsonSchema]] instance
   */
  def toExpr[T](schema: SchemaExpr[T], noInline: Boolean = false)(using q: Quotes, tt: Type[T]): Expr[JsonSchema[T]] = {
    import q.reflect.*

    schema match {
      case SchemaExpr.Runtime(expr) => expr
      case inl: SchemaExpr.Inlined[T] =>
        var code = generateCode(inl)

        if (!noInline && inl.canInline) {
          val tpe = toType(inl)
          code = Typed(code.asTerm, TypeTree.of(using tpe)).asExprOf[JsonSchema[T]]
        }

        code
    }
  }
}
