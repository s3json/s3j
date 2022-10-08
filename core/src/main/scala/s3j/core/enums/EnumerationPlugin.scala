package s3j.core.enums

import s3j.annotations.{discriminator, objectEnum}
import s3j.core.enums.modifiers.{DiscriminatorModifier, ObjectEnumModifier}
import s3j.macros.GenerationContext.{GenerationOutcome, GenerationUnsupported}
import s3j.macros.{GenerationContext, Plugin, PluginContext}
import s3j.macros.modifiers.{ModifierParser, ModifierParsers, ModifierSet}

import scala.quoted.{Quotes, Type, quotes}

class EnumerationPlugin extends Plugin {
  def name: String = "Enumerations plugin"

  override def modifierParser(using PluginContext): ModifierParser = ModifierParser.builder
    .parse[objectEnum](ObjectEnumModifier)
    .parseFn[discriminator](ModifierParsers.parseString(DiscriminatorModifier.apply))
    .build()

  override def generate[T](modifiers: ModifierSet)(using Quotes, GenerationContext, Type[T]): GenerationOutcome = {
    import quotes.reflect.*
    val typeSym = TypeRepr.of[T].typeSymbol
    if (!typeSym.isClassDef || !typeSym.flags.is(Flags.Sealed)) GenerationUnsupported
    else {
      val asString: Boolean = !typeSym.children.exists(_.isClassDef) && !modifiers.contains(ObjectEnumModifier.key)

      if (asString) new StringyCandidate(modifiers)
      else new ObjectCandidate(modifiers)
    }
  }
}
