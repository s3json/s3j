package s3j.macros.generic

import s3j.annotations.requireImplicit
import s3j.macros.modifiers.{BuiltinModifiers, ModifierParser}
import s3j.macros.{Plugin, PluginContext}

/** Not actually a plugin (does not generate anything), but acts as a dummy instance for PluginContainer */
class BuiltinsPlugin extends Plugin {
  def name: String = "s3j builtin plugin"

  /** @return Modifier parser for this plugin */
  override def modifierParser(using c: PluginContext): ModifierParser = ModifierParser.builder
    .parse[requireImplicit](BuiltinModifiers.RequireImplicit)
    .build()
}
