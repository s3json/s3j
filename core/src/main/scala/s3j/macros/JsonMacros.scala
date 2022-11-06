package s3j.macros

import s3j.core.binary.BinaryPlugin
import s3j.core.casecls.CaseClassPlugin
import s3j.core.enums.EnumerationPlugin
import s3j.core.primitives.PrimitivesPlugin
import s3j.macros.generic.{BuiltinsPlugin, GenerationMode, PluginContextImpl}
import s3j.macros.modifiers.BuiltinModifiers.InspectCodeModifier
import s3j.macros.utils.MacroUtils

import java.util.{Base64, ServiceLoader}
import scala.collection.mutable
import scala.quoted.*
import scala.util.control.NonFatal

object JsonMacros {
  /**
   * @return Derived JSON codec
   */
  inline def derived[T]: T = ${ derivedMacro[T] }

  /**
   * Create new s3j context to generate codecs with it.
   *
   * @param q Primary quotes object to use
   * @return  Fresh plugin context
   */
  def createContext(using q: Quotes): FreshPluginContext = {
    val ctx = new PluginContextImpl

    // Load builtin plugins:
    ctx.loadPlugin[BuiltinsPlugin]()
    ctx.loadPlugin[CaseClassPlugin]()
    ctx.loadPlugin[EnumerationPlugin]()
    ctx.loadPlugin[BinaryPlugin]()
    ctx.loadPlugin[PrimitivesPlugin]()

    // Load user-configured plugins:
    for (plugin <- MacroUtils.macroSettings(MacroUtils.UsePluginPrefix)) {
      ctx.loadPlugin(plugin)
    }

    ctx
  }

  private def derivedMacro[T](using q: Quotes, tt: Type[T]): Expr[T] = {
    import q.reflect.*
    val ctx = createContext

    val decodedMode = GenerationMode.decode[T]
    val effectiveModifiers = ctx.symbolModifiers(TypeRepr.of[decodedMode.I].typeSymbol).inherited ++
      ctx.symbolModifiers(Symbol.spliceOwner).own

    val root = ctx.generate[decodedMode.I](decodedMode.mode, effectiveModifiers)
    val result = ctx.result(root.raw)
    
    if (effectiveModifiers.contains(InspectCodeModifier)) {
      report.errorAndAbort("\u001b[1;32mGenerated code (@inspectCode annotation):\u001b[0m " +
        result.asTerm.show(using Printer.TreeAnsiCode))
    }

    result.asExprOf[T]
  }
}
