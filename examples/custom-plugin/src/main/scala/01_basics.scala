import s3j.macros.{GenerationContext, Plugin, PluginContext}

import scala.annotation.StaticAnnotation
import s3j.annotations.usePlugin
import s3j.macros.GenerationContext.{GenerationOutcome, GenerationUnsupported}
import s3j.macros.modifiers.{Modifier, ModifierKey, ModifierParser, ModifierSet}

import scala.quoted.{Quotes, Type}

class MeowingPlugin extends Plugin {
  def name: String = "Meowing plugin"

  override def modifierParser(using c: PluginContext): ModifierParser = ModifierParser.builder
    .parse[meowingString](MeowingModifier)
    .build()

  override def generate[T](modifiers: ModifierSet)(using Quotes, GenerationContext, Type[T]): GenerationOutcome =
    GenerationUnsupported
}

/** When applied to a field, causes string to be serialized in meowing way */
@usePlugin[MeowingPlugin]
class meowingString extends StaticAnnotation

case object MeowingModifier extends Modifier {
  val key: ModifierKey[_ <: MeowingModifier.this.type] = ModifierKey("meowingString")
  override def suppressImplicitSearch: Boolean = false
}
