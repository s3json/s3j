package s3j.macros.generic

import s3j.macros.FreshPluginContext.StackHandle
import s3j.macros.PluginContext.ExtensionRegistration
import s3j.macros.{FreshPluginContext, Plugin, PluginCapability, PluginContext}
import s3j.macros.modifiers.{Modifier, ModifierParser}
import s3j.macros.traits.ErrorReporting
import s3j.macros.utils.{MacroUtils, QualifiedName, ReportingUtils}

import scala.collection.mutable
import scala.quoted.runtime.impl.QuotesImpl
import scala.quoted.{Expr, Quotes, Type}
import scala.util.control.NonFatal

private[macros] class PluginContextImpl(using val q: Quotes)
extends FreshPluginContext with ModifierParserImpl with GenerationContextImpl with GenerationStateImpl {
  protected val qi: QuotesImpl & q.type = q.asInstanceOf[QuotesImpl & q.type]
  import q.reflect.*

  protected class PluginContainer(val className: String, val instance: Plugin) {
    val name: String = instance.name
    val modifierParser: ModifierParser =
      try instance.modifierParser(using PluginContextImpl.this)
      catch { case NonFatal(e) => ReportingUtils.reportException(s"Failed get modifier parser for plugin $className", e) }

    val capabilities: Set[PluginCapability] = instance.capabilities
    val supportedModifiers: Set[String] = modifierParser.supportedTypes
  }

  protected val _plugins: mutable.Map[String, PluginContainer] = mutable.HashMap.empty
  protected var _pluginContainers: Set[PluginContainer] = Set.empty
  protected var _pluginInstances: Set[Plugin] = Set.empty
  protected val _extensions: mutable.Map[Extensions.Key[_], Set[ExtensionRegistration[Any]]] = mutable.HashMap.empty
  protected val _modifiers: mutable.Map[String, PluginContainer] = mutable.HashMap.empty
  protected val _stack: CodecStackBuilder = new CodecStackBuilder()

  val report: ErrorReporting = ErrorReporting.fromQuotes

  def loadPlugin(className: String): Unit = {
    if (_plugins.contains(className)) {
      return
    }

    val instance: Plugin =
      try Class.forName(className).getConstructor().newInstance().asInstanceOf[Plugin]
      catch { case NonFatal(e) => ReportingUtils.reportException(s"Failed to load plugin $className", e) }

    val container = new PluginContainer(className, instance)
    if (container.supportedModifiers.exists(_modifiers.contains)) {
      val errorMsg = new mutable.StringBuilder()
      errorMsg ++= s"Plugin '$className' register conflicting modifier parsers:\n"

      for (mod <- container.supportedModifiers if _modifiers.contains(mod)) {
        errorMsg ++= s" - modifier @" ++= mod ++= s" is handled by plugin " ++= _modifiers(mod).className ++= "\n"
      }

      report.errorAndAbort(errorMsg.result())
    }

    checkRegisteredModifiers(className, container.supportedModifiers)

    // Update data structures:

    _plugins.put(className, container)
    _pluginContainers += container
    _pluginInstances += instance
    for (mod <- container.supportedModifiers) _modifiers.put(mod, container)
    for ((k, vs) <- container.instance.extensions.untypedMap; v <- vs) {
      _extensions.put(k, _extensions.getOrElse(k, Set.empty) + ExtensionRegistration(className, instance, v))
    }
  }

  def loadPlugin[T <: Plugin]()(using qn: QualifiedName[T]): Unit =
    loadPlugin(qn.name)

  def plugins: Set[Plugin] = _pluginInstances

  def extensions[T](key: Extensions.Key[T]): Set[ExtensionRegistration[T]] =
    _extensions.getOrElse(key, Set.empty).asInstanceOf[Set[ExtensionRegistration[T]]]

  def addStackEntry[T](name: String)(using Type[T]): StackHandle[T] =
    _stack.addEntry[T](name)

  def result[T](result: Expr[T])(using Type[T]): Expr[T] = _stack.result(result)
}
