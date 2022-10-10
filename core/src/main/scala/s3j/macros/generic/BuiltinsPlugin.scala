package s3j.macros.generic

import s3j.annotations.requireImplicit
import s3j.macros.modifiers.{BuiltinModifiers, ModifierParser}
import s3j.macros.{Plugin, PluginContext}

import scala.quoted.Quotes

/** Not actually a plugin (does not generate anything), but acts as a dummy instance for PluginContainer */
class BuiltinsPlugin extends Plugin {
  def name: String = "s3j builtin plugin"

  override def implicitLocations(using q: Quotes): Set[q.reflect.Symbol] = {
    import q.reflect.*
    Set(
      Symbol.requiredModule("s3j.format.BasicFormats"),
      Symbol.requiredModule("s3j.format.CollectionFormats")
    )
  }

  /** @return Modifier parser for this plugin */
  override def modifierParser(using c: PluginContext): ModifierParser = ModifierParser.builder
    .parse[requireImplicit](BuiltinModifiers.RequireImplicit)
    .build()
}
