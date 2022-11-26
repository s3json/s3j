package s3j.core.casecls

import s3j.annotations.naming.*
import s3j.annotations.{allowUnknownKeys, failUnknownKeys, inlineObject, key, keyPrefix, nullOption, restFields}
import s3j.core.casecls.impl.{CaseClassCandidate, InlineObjectExtension, OptionExtension, PlainFieldExtension, RestFieldsExtension}
import s3j.core.casecls.modifiers.{FieldCaseModifier, FieldKeyModifier, InlineObjectModifier, KeyPrefixModifier, NullOptionModifier, RestFieldsModifier, UnknownKeysModifier}
import s3j.core.enums.modifiers.EnumCaseModifier
import s3j.macros.GenerationContext.{GenerationOutcome, GenerationUnsupported}
import s3j.macros.generic.Extensions
import s3j.macros.modifiers.ModifierParser.{AnnotationModifier, StoredModifier, TextModifier}
import s3j.macros.modifiers.{Modifier, ModifierContext, ModifierParser, ModifierParsers, ModifierSet}
import s3j.macros.{GenerationContext, Plugin, PluginContext}

import scala.quoted.{Quotes, Type, quotes}

class CaseClassPlugin extends Plugin {
  def name: String = "Case class plugin"

  override def extensions: Extensions = Extensions(
    CaseClassExtension ~> new PlainFieldExtension,
    CaseClassExtension ~> new RestFieldsExtension,
    CaseClassExtension ~> new OptionExtension,
    CaseClassExtension ~> new InlineObjectExtension
  )

  override def modifierParser(using PluginContext): ModifierParser = ModifierParser.builder
    .parse[allowUnknownKeys](UnknownKeysModifier(true))
    .parse[failUnknownKeys](UnknownKeysModifier(false))
    .parse[restFields](RestFieldsModifier)
    .parse[nullOption](NullOptionModifier)
    .parse[inlineObject](InlineObjectModifier)
    .parseFn[key](ModifierParsers.parseString(FieldKeyModifier.apply))
    .parseFn[keyPrefix](ModifierParsers.parseString(KeyPrefixModifier.apply))
    .build()

  override def generate[T](modifiers: ModifierSet)(using Quotes, GenerationContext, Type[T]): GenerationOutcome = {
    import quotes.reflect.*
    val sym = TypeRepr.of[T].typeSymbol
    if (sym.isClassDef && sym.flags.is(Flags.Case)) new CaseClassCandidate[T](modifiers).candidate
    else GenerationUnsupported
  }
}
