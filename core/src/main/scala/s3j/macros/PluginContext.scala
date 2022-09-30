package s3j.macros

import s3j.macros.PluginContext.{ExtensionRegistration, SymbolModifiers}
import s3j.macros.generic.{Extensions, GenerationMode, PluginCtxUtils}
import s3j.macros.modifiers.{Modifier, ModifierContext, ModifierKey, ModifierSet}
import s3j.macros.traits.BasicPluginContext
import s3j.macros.utils.QualifiedName

import scala.annotation.targetName
import scala.collection.mutable
import scala.quoted.{Expr, Quotes, Type, quotes}

object PluginContext {
  /** Extension instance along with owning plugin metadata */
  case class ExtensionRegistration[T](pluginClass: String, pluginInstance: Plugin, instance: T)

  trait SymbolModifiers {
    /** @return Modifiers for parent symbol, if one exists */
    def parent: Option[SymbolModifiers]

    /** @return Modifiers specific to this symbol */
    def own: ModifierSet

    /** @return Merged modifiers for this symbol and parent */
    def inherited: ModifierSet
  }
}

trait PluginContext extends BasicPluginContext {
  type T
  
  /** @return Root generated type */
  def generatedType: Type[T]

  /** @return Generation mode decoded from requested type class */
  def generationMode: GenerationMode
}
