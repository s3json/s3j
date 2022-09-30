package s3j.core.casecls

import s3j.annotations.{allowUnknownKeys, failUnknownKeys, key, restFields}
import s3j.core.casecls.impl.{CaseClassGenerator, PlainFieldExtension, RestFieldsExtension}
import s3j.core.casecls.modifiers.{RestFieldsModifier, UnknownKeysModifier}
import s3j.macros.GenerationContext.{GenerationOutcome, GenerationUnsupported}
import s3j.macros.generic.Extensions
import s3j.macros.modifiers.{ModifierParser, ModifierSet}
import s3j.macros.{GenerationContext, Plugin, PluginContext}

import scala.quoted.{Quotes, Type, quotes}

class CaseClassPlugin extends Plugin {
  def name: String = "Case class plugin"

  override def extensions: Extensions = Extensions(
    CaseClassExtension.key ~> new PlainFieldExtension,
    CaseClassExtension.key ~> new RestFieldsExtension
  )

  override def modifierParser(using PluginContext): ModifierParser = ModifierParser.builder
    .parse[allowUnknownKeys](UnknownKeysModifier(true))
    .parse[failUnknownKeys](UnknownKeysModifier(false))
    .parse[restFields](RestFieldsModifier)
    .build()

  override def generate[T](modifiers: ModifierSet)(using Quotes, GenerationContext, Type[T]): GenerationOutcome = {
    import quotes.reflect.*
    val sym = TypeRepr.of[T].typeSymbol
    if (sym.isClassDef && sym.flags.is(Flags.Case)) new CaseClassGenerator[T](modifiers).candidate
    else GenerationUnsupported
  }
}
