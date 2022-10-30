package s3j.macros.schema.impl

import s3j.macros.schema.SchemaExpr
import s3j.schema.JsonSchema
import s3j.schema.model.SchemaDocument
import s3j.*

import scala.quoted.{Expr, Quotes, Type}
import scala.util.control.NonFatal

private[schema] object SchemaParser {
  def parseDeps(using q: Quotes)(deps: q.reflect.TypeRepr): Seq[SchemaExpr[?]] = {
    import q.reflect.*

    if (deps <:< TypeRepr.of[Unit]) Nil
    else if (deps <:< TypeRepr.of[JsonSchema[?]]) fromType(deps) :: Nil
    else ???
  }

  def fromType(using q: Quotes)(t: q.reflect.TypeRepr): SchemaExpr[?] = {
    import q.reflect.*

    t.widen match {
      case baseTpe @ AppliedType(TypeRef(_, "InlineSchema"), List(inner, json, deps)) =>
        val jsonStr = json match {
          case ConstantType(StringConstant(str)) => str
          case _ => report.errorAndAbort("Failed to extract JSON from InlineSchema type: not a string constant: " +
            json.show + " in type " + baseTpe.show)
        }

        val doc =
          try jsonStr.fromJson[SchemaDocument]
          catch {
            case NonFatal(e) => report.errorAndAbort("Failed to parse JSON schema document from InlineSchema type: " +
              baseTpe.show + ": " + e)
          }

        type T
        given Type[T] = inner.asType.asInstanceOf[Type[T]]
        SchemaExpr.Inlined[T](
          document = doc,
          definitions = parseDeps(deps),
          shouldInline = true
        )
    }
  }

  def fromExpr[T](expr: Expr[JsonSchema[T]])(using q: Quotes, tt: Type[T]): SchemaExpr[T] = {
    import q.reflect.*

    expr.asTerm.tpe.widen match {
      case base @ AppliedType(TypeRef(_, "InlineSchema"), _) =>
        fromType(base).asInstanceOf[SchemaExpr[T]]

      case _ => SchemaExpr.Runtime(expr)
    }
  }
}
