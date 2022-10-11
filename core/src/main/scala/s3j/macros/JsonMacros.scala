package s3j.macros

import s3j.core.binary.BinaryPlugin
import s3j.core.casecls.CaseClassPlugin
import s3j.core.enums.EnumerationPlugin
import s3j.macros.generic.{BuiltinsPlugin, PluginContextImpl}

import java.util.{Base64, ServiceLoader}
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
    ctx.loadPlugin[EnumerationPlugin]()
    ctx.loadPlugin[BinaryPlugin]()

    val effectiveModifiers = ctx.symbolModifiers(ctx.typeSymbol).inherited ++
      ctx.symbolModifiers(Symbol.spliceOwner).own

    val r = ctx.buildFinalBlock(() => ctx.generateRoot(TypeRepr.of(using ctx.generatedType), effectiveModifiers))
    if (ctx.inspectCode) {
      report.errorAndAbort("\u001b[1;32mGenerated code (@inspectCode annotation):\u001b[0m " +
        r.show(using Printer.TreeAnsiCode))
    }

    r.asExprOf[T]
  }
}
