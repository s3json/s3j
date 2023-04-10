package s3j.macros.traits

import s3j.macros.Plugin
import s3j.macros.PluginContext.{ExtensionRegistration, SymbolModifiers}
import s3j.macros.generic.{Extensions, GenerationMode}
import s3j.macros.utils.QualifiedName

import scala.quoted.Quotes

/** Basic plugin context methods to use in generation contexts */
trait BasicPluginContext {
  /** Error reporting methods */
  val report: ErrorReporting
  
  /** @return Parsed modifier set for a symbol */
  def symbolModifiers(using q: Quotes)(sym: q.reflect.Symbol): SymbolModifiers

  // Plugin management: ================================================================================================

  /** Load plugin with specified class name */
  def loadPlugin(className: String): Unit

  /** Load plugin by it's type */
  def loadPlugin[T <: Plugin]()(using QualifiedName[T]): Unit
  
  /** Use already instantiated plugin */
  def usePlugin(plugin: Plugin): Unit
  
  /** @return List of currently loaded plugins */
  def plugins: Set[Plugin]

  /** @return Currently registered extension instances for the key */
  def extensions[T](key: Extensions.Key[T]): Set[ExtensionRegistration[T]]
}
