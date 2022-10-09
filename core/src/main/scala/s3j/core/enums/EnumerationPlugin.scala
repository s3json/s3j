package s3j.core.enums

import s3j.annotations.{allowBuffering, discriminator, discriminatorField, objectEnum, stringyCases}
import s3j.core.enums.modifiers.*
import s3j.macros.GenerationContext.{GenerationOutcome, GenerationUnsupported}
import s3j.macros.{GenerationContext, Plugin, PluginContext}
import s3j.macros.modifiers.{ModifierParser, ModifierParsers, ModifierSet}

import scala.quoted.{Quotes, Type, quotes}

class EnumerationPlugin extends Plugin {
  def name: String = "Enumerations plugin"

  override def modifierParser(using PluginContext): ModifierParser = ModifierParser.builder
    .parse[objectEnum](EnumObjectModifier(EnumObjectModifier.Behavior.ForceObject))
    .parse[stringyCases](EnumObjectModifier(EnumObjectModifier.Behavior.AllowStrings))
    .parse[allowBuffering](AllowBufferingModifier)
    .parseFn[discriminator](ModifierParsers.parseString(DiscriminatorModifier.apply))
    .parseFn[discriminatorField](ModifierParsers.parseString(DiscriminatorFieldModifier.apply))
    .build()

  override def generate[T](modifiers: ModifierSet)(using Quotes, GenerationContext, Type[T]): GenerationOutcome = {
    import quotes.reflect.*
    val typeSym = TypeRepr.of[T].typeSymbol
    if (!typeSym.isClassDef || !typeSym.flags.is(Flags.Sealed)) GenerationUnsupported
    else {
      if (typeSym.children.isEmpty) {
        implicitly[GenerationContext].report.errorAndAbort("Could not generate enumeration format for class " +
          "with no children or enum cases")
      }
      
      new CandidateImpl(modifiers)
    }
  }
}
