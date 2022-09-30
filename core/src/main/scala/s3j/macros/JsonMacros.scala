package s3j.macros

import s3j.core.casecls.CaseClassPlugin
import s3j.macros.generic.{BuiltinsPlugin, PluginContextImpl}

import java.util.Base64
import scala.collection.mutable
import scala.quoted.*
import scala.util.control.NonFatal

object JsonMacros {
  inline def derived[T]: T = ${ derivedMacro[T] }

  private def derivedMacro[T](using q: Quotes, tt: Type[T]): Expr[T] = {
    import q.reflect.*
    val ctx = new PluginContextImpl[q.type](TypeRepr.of[T])
    
    // Load builtin plugins:
    ctx.loadPlugin[BuiltinsPlugin]()
    ctx.loadPlugin[CaseClassPlugin]()

    val effectiveModifiers = ctx.symbolModifiers(ctx.typeSymbol).inherited ++
      ctx.symbolModifiers(Symbol.spliceOwner).own

    val r = ctx.buildFinalBlock(() => ctx.generateRoot(TypeRepr.of(using ctx.generatedType), effectiveModifiers))

    println(r.show(using Printer.TreeCode))

    r.asExpr.asInstanceOf[Expr[T]]
  }
}
