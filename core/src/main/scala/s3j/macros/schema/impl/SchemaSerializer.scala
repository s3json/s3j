package s3j.macros.schema.impl

import s3j.macros.schema.SchemaExpr
import s3j.schema.{JsonSchema, InlineSchema}
import s3j.*
import s3j.ast.JsValue

import scala.quoted.{Expr, Quotes, Type, Varargs}

private[schema] object SchemaSerializer {
  def generateCode[T](inl: SchemaExpr.Inlined[T])(using Quotes, Type[T]): Expr[JsonSchema[T]] = {
    def definitions(using Quotes): Expr[Seq[() => JsonSchema[?]]] = {
      val exprs = inl.definitions.map { e =>
        given Type[e.Underlying] = e.valueType
        '{ () => ${ toExpr(e, noInline = true) } }
      }

      if (exprs.nonEmpty) '{ Vector(${ Varargs(exprs) }: _*) }
      else '{ Nil }
    }

    def summonEncoder[T](using q: Quotes, tt: Type[T]): Expr[JsonEncoder[T]] = {
      import q.reflect.*
      Implicits.search(TypeRepr.of[JsonEncoder[T]]) match {
        case s: ImplicitSearchSuccess => s.tree.asExprOf[JsonEncoder[T]]
        case f: ImplicitSearchFailure =>
          report.errorAndAbort("failed to summon encoder for " + Type.show[T])
      }
    }

    def defaultValue(using Quotes): Expr[Option[() => JsValue]] =
      inl.defaultValue match {
        case Some(v) => '{ Some(() => ${ summonEncoder[T] }.encodeValue($v) ) }
        case None => '{ None }
      }

    def exampleValues(using Quotes): Expr[Option[() => Seq[JsValue]]] =
      inl.exampleValues.filter(_.nonEmpty) match {
        case Some(vs) =>
          val es = vs.map(v => '{ ${ summonEncoder[T] }.encodeValue($v) })
          '{ Some(() => Vector(${ Varargs(es) }:_*)) }

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

  def toType[T, R <: JsonSchema[T]](schema: SchemaExpr[T])(using q: Quotes): Type[R] = {
    import q.reflect.*
    given Type[T] = schema.valueType
    if (!schema.canInline) {
      return Type.of[JsonSchema[T]].asInstanceOf[Type[R]]
    }

    val inl: SchemaExpr.Inlined[T] = schema.asInstanceOf[SchemaExpr.Inlined[T]]
    val jsonType = ConstantType(StringConstant(inl.document.toJsonString))
    val depsType =
      if (inl.definitions.isEmpty) TypeRepr.of[Unit]
      else {
        val deps = inl.definitions.map { e => TypeRepr.of(using toType(e)) }

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
  def toExpr[T](schema: SchemaExpr[T], noInline: Boolean = false)(using q: Quotes): Expr[JsonSchema[T]] = {
    given Type[T] = schema.valueType
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
