package s3j.macros.generic

import s3j.macros.PluginContext.ExtensionRegistration
import s3j.macros.{Plugin, PluginContext}
import s3j.macros.modifiers.{Modifier, ModifierParser}
import s3j.macros.traits.ErrorReporting
import s3j.macros.utils.{MacroUtils, QualifiedName, ReportingUtils}

import scala.collection.mutable
import scala.quoted.{Quotes, Type}
import scala.util.control.NonFatal

private[macros] class PluginContextImpl[Q <: Quotes](using val q: Q)(val _generatedType: q.reflect.TypeRepr)
extends PluginContext with ModifierParserImpl with GenerationContextImpl with GenerationStateImpl {
  import q.reflect.*

  protected class PluginContainer(val className: String, val instance: Plugin) {
    val name: String = instance.name
    val modifierParser: ModifierParser =
      try instance.modifierParser(using PluginContextImpl.this)
      catch { case NonFatal(e) => ReportingUtils.reportException(s"Failed get modifier parser for plugin $className", e) }

    val supportedModifiers: Set[String] = modifierParser.supportedTypes
  }

  protected val _plugins: mutable.Map[String, PluginContainer] = mutable.HashMap.empty
  protected var _pluginContainers: Set[PluginContainer] = Set.empty
  protected var _pluginInstances: Set[Plugin] = Set.empty
  protected val _extensions: mutable.Map[Extensions.Key[_], Set[ExtensionRegistration[Any]]] = mutable.HashMap.empty
  protected val _modifiers: mutable.Map[String, PluginContainer] = mutable.HashMap.empty

  val report: ErrorReporting = ErrorReporting.fromQuotes

  val (generationMode: GenerationMode, _rootType: TypeRepr) = GenerationMode.decode(_generatedType)
  val generatedType: Type[T] = _rootType.asType.asInstanceOf[Type[T]]
  val typeSymbol: Symbol = _rootType.typeSymbol
  
  val inspectCode: Boolean = typeSymbol.annotations.map(parseAnnotation)
    .exists(_.fullName == "s3j.annotations.inspectCode")

  private var _termCounter: Int = 0

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

  def freshName(name: String): String = {
    val r = name + "$s3j_" + _termCounter
    _termCounter += 1
    r
  }

  def freshName(): String = freshName("macro")
}
