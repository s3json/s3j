package s3j.schema.impl

import s3j.schema.{JsonSchema, InlineSchema}

import scala.quoted.{Expr, Quotes, Type}

object InlineSchemaMacros {
  private def isInlineSchema(using q: Quotes)(tpe: q.reflect.TypeRepr): Boolean = {
    import q.reflect.*

    tpe match {
      case AppliedType(base, _) => base =:= TypeRepr.of[InlineSchema]
      case _ => tpe =:= TypeRepr.of[InlineSchema]
    }
  }

  private def extractString(using q: Quotes)(tpe: q.reflect.TypeRepr): String = {
    import q.reflect.*

    tpe match {
      case ConstantType(StringConstant(str)) => str
      case _ => report.errorAndAbort("Failed to extract string constant from: " +
        tpe.show(using Printer.TypeReprStructure))
    }
  }

  private def generatePolymorphic[T](using q: Quotes, tt: Type[T])
                                    (method: q.reflect.Symbol, tpe: q.reflect.TypeRepr): Expr[JsonSchema[T]] =
  {
    import q.reflect.*
    val PolyType(_, _, MethodType(methodArgs, _, innerType)) = tpe: @unchecked

    val (json, depType) = innerType match {
      case AppliedType(base, List(_, j, d)) if isInlineSchema(base) => extractString(j.dealias) -> d
      case _ => report.errorAndAbort("Failed to match inner polymorphic type: " + innerType.show)
    }

    val deps: List[TypeRepr] =
      if (depType.isTupleN) depType.typeArgs
      else if (depType == TypeRepr.of[Unit]) Nil
      else depType match {
        case t: ParamRef => t :: Nil
        case _ => report.errorAndAbort("Failed to extract list of dependencies from " +
          depType.show(using Printer.TypeReprStructure))
      }

    val params: Map[String, Symbol] = method.paramSymss.flatten.filterNot(_.isType).map(s => s.name -> s).toMap

    val depExprs: List[Expr[JsonSchema[?]]] = deps.map {
      case ParamRef(_, idx) =>
        val s = params(methodArgs(idx))
        Ref(s).asExprOf[JsonSchema[?]]

      case other => report.errorAndAbort("Failed to extract parameter from type " +
        other.show(using Printer.TypeReprStructure))
    }

    '{ new JsonSchema[T](${ Expr(json) }, () => Vector(${ Expr.ofSeq(depExprs) }:_*), true) }
  }

  private def generateSimple[T](using q: Quotes, tt: Type[T])(tpe: q.reflect.TypeRepr): Expr[JsonSchema[T]] = {
    import q.reflect.*
    val AppliedType(_, List(_, jsonTpe, _)) = tpe: @unchecked
    val json = extractString(jsonTpe)

    '{ new JsonSchema[T](${ Expr(json) }, shouldInline = true) }
  }

  def generateInline[T](using q: Quotes, tt: Type[T]): Expr[JsonSchema[T]] = {
    import q.reflect.*

    var defn = Symbol.spliceOwner.owner // skip first owner
    while (defn != Symbol.noSymbol && !(defn.isValDef || defn.flags.is(Flags.Method))) defn = defn.maybeOwner
    if (defn == Symbol.noSymbol) report.errorAndAbort("Failed to determine macro expansion owner")

    Ident(defn.termRef).tpe.widen match {
      case tpe @ PolyType(_, _, MethodType(_, _, innerType)) if isInlineSchema(innerType) =>
        generatePolymorphic[T](defn, tpe)

      case tpe if isInlineSchema(tpe) => generateSimple(tpe)
      case tpe => report.errorAndAbort("Failed to match inline schema type: " + tpe.show)
    }
  }
}
