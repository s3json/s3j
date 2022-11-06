package s3j.macros.generic

import s3j.annotations.naming.*
import s3j.annotations.schema.*
import s3j.annotations.{requireImplicit, inspectCode}
import s3j.core.casecls.modifiers.FieldCaseModifier
import s3j.core.enums.modifiers.EnumCaseModifier
import s3j.macros.modifiers.ModifierParser.{AnnotationModifier, StoredModifier, TextModifier}
import s3j.macros.modifiers.{BuiltinModifiers, Modifier, ModifierContext, ModifierParser, ModifierParsers, ModifierSet}
import s3j.macros.schema.modifiers.{SchemaDeprecatedModifier, SchemaDescriptionModifier, SchemaHiddenModifier, SchemaTitleModifier}
import s3j.macros.{Plugin, PluginContext}

import scala.quoted.{Quotes, Type, quotes}

/** Not actually a plugin (does not generate anything), but acts as a dummy instance for PluginContainer */
class BuiltinsPlugin extends Plugin {
  def name: String = "s3j builtin plugin"

  override def implicitBehavior[T](modifiers: ModifierSet)(using Quotes, PluginContext, Type[T]): ImplicitBehavior = {
    import quotes.reflect.*
    ImplicitBehavior.Extend(
      Symbol.requiredModule("s3j.format.BasicFormats"),
      Symbol.requiredModule("s3j.format.CollectionFormats"),
      Symbol.requiredModule("s3j.schema.BasicSchemas"),
      Symbol.requiredModule("s3j.schema.CollectionSchemas")
    )
  }

  private type ParseFn = PartialFunction[StoredModifier, Modifier]

  private def parseCaseConvention(using c: PluginContext)(cc: CaseConvention): ParseFn = {
    case ann: AnnotationModifier =>
      if (ann.context == ModifierContext.Enum || ann.context == ModifierContext.EnumCase)
        EnumCaseModifier(cc)
      else
        FieldCaseModifier(cc)

    case _: TextModifier => c.report.errorAndAbort("Case convention annotations are ambiguous when specified " +
        "as text. Please use '@defaultFieldCase' and '@defaultEnumCase' instead.")
  }

  private def parseCaseConvention(name: String): CaseConvention =
    CaseConvention.PascalCase.transform(name) match {
      case "NoConvention"         => CaseConvention.NoConvention
      case "CamelCase"            => CaseConvention.CamelCase
      case "SnakeCase"            => CaseConvention.SnakeCase
      case "ScreamingSnakeCase"   => CaseConvention.ScreamingSnakeCase
      case "KebabCase"            => CaseConvention.KebabCase
      case "CapitalizedKebabCase" => CaseConvention.CapitalizedKebabCase
      case "PascalCase"           => CaseConvention.PascalCase
      case other => throw new IllegalArgumentException("Invalid case convention name: " + other)
    }

  private def parseAnyConvention(using c: PluginContext, q: Quotes)(f: CaseConvention => Modifier): ParseFn = {
    case ann: AnnotationModifier =>
      import q.reflect.*
      ann.args.head.asTerm match {
        case Select(Ident(_), name)         => f(parseCaseConvention(name))
        case Literal(StringConstant(name))  => f(parseCaseConvention(name))
        case other => throw new IllegalArgumentException("Unmatched case convention tree: " +
          other.show(using Printer.TreeStructure))
      }

    case text: TextModifier => f(parseCaseConvention(text.content))
  }

  /** @return Modifier parser for this plugin */
  override def modifierParser(using c: PluginContext): ModifierParser = ModifierParser.builder
    .parse[requireImplicit](BuiltinModifiers.RequireImplicit)
    .parse[inspectCode](BuiltinModifiers.InspectCodeModifier)
    .parseFn[capitalizedKebabCase](parseCaseConvention(CaseConvention.CapitalizedKebabCase))
    .parseFn[kebabCase](parseCaseConvention(CaseConvention.KebabCase))
    .parseFn[pascalCase](parseCaseConvention(CaseConvention.PascalCase))
    .parseFn[camelCase](parseCaseConvention(CaseConvention.CamelCase))
    .parseFn[screamingSnakeCase](parseCaseConvention(CaseConvention.ScreamingSnakeCase))
    .parseFn[snakeCase](parseCaseConvention(CaseConvention.SnakeCase))
    .parseFn[defaultFieldCase](parseAnyConvention(FieldCaseModifier(_)))
    .parseFn[defaultEnumCase](parseAnyConvention(EnumCaseModifier(_)))
    .parse[schemaHidden](SchemaHiddenModifier)
    .parse[schemaDeprecated](SchemaDeprecatedModifier)
    .parseFn[schemaTitle](ModifierParsers.parseString(SchemaTitleModifier(_)))
    .parseFn[schemaDescription](ModifierParsers.parseString(SchemaDescriptionModifier(_)))
    .build()
}
