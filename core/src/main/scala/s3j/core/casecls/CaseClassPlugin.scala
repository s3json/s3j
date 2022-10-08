package s3j.core.casecls

import s3j.annotations.naming.*
import s3j.annotations.{allowUnknownKeys, failUnknownKeys, key, nullOption, restFields}
import s3j.core.casecls.impl.{CaseClassGenerator, OptionExtension, PlainFieldExtension, RestFieldsExtension}
import s3j.core.casecls.modifiers.{FieldCaseModifier, FieldKeyModifier, NullOptionModifier, RestFieldsModifier, UnknownKeysModifier}
import s3j.macros.GenerationContext.{GenerationOutcome, GenerationUnsupported}
import s3j.macros.generic.Extensions
import s3j.macros.modifiers.ModifierParser.{AnnotationModifier, StoredModifier}
import s3j.macros.modifiers.{Modifier, ModifierContext, ModifierParser, ModifierSet}
import s3j.macros.{GenerationContext, Plugin, PluginContext}

import scala.quoted.{Quotes, Type, quotes}

class CaseClassPlugin extends Plugin {
  def name: String = "Case class plugin"

  override def extensions: Extensions = Extensions(
    CaseClassExtension.key ~> new PlainFieldExtension,
    CaseClassExtension.key ~> new RestFieldsExtension,
    CaseClassExtension.key ~> new OptionExtension
  )

  private def parseCaseConvention(cc: CaseConvention): PartialFunction[StoredModifier, Modifier] = {
    case ann: AnnotationModifier =>
      if (ann.context == ModifierContext.Enum || ann.context == ModifierContext.EnumCase)
        ???
      else
        FieldCaseModifier(cc)
  }

  override def modifierParser(using PluginContext): ModifierParser = ModifierParser.builder
    .parse[allowUnknownKeys](UnknownKeysModifier(true))
    .parse[failUnknownKeys](UnknownKeysModifier(false))
    .parse[restFields](RestFieldsModifier)
    .parse[nullOption](NullOptionModifier)
    .parseFn[capitalizedKebabCase](parseCaseConvention(CaseConvention.CapitalizedKebabCase))
    .parseFn[kebabCase](parseCaseConvention(CaseConvention.KebabCase))
    .parseFn[pascalCase](parseCaseConvention(CaseConvention.PascalCase))
    .parseFn[screamingSnakeCase](parseCaseConvention(CaseConvention.ScreamingSnakeCase))
    .parseFn[snakeCase](parseCaseConvention(CaseConvention.SnakeCase))
    .parseFn[key] {
      case ann: AnnotationModifier =>
        import quotes.reflect.*
        ann.args match {
          case arg :: Nil if arg.isExprOf[String] => FieldKeyModifier(arg.asExprOf[String].valueOrAbort)
          case _ => report.errorAndAbort("@key annotation must contain a string literal as argument")
        }
    }
    .build()

  override def generate[T](modifiers: ModifierSet)(using Quotes, GenerationContext, Type[T]): GenerationOutcome = {
    import quotes.reflect.*
    val sym = TypeRepr.of[T].typeSymbol
    if (sym.isClassDef && sym.flags.is(Flags.Case)) new CaseClassGenerator[T](modifiers).candidate
    else GenerationUnsupported
  }
}
