import s3j.macros.{GenerationContext, Plugin, PluginContext}

import scala.annotation.StaticAnnotation
import s3j.annotations.usePlugin
import s3j.format.{BasicFormats, JsonFormat}
import s3j.macros.GenerationContext.{GenerationCandidate, GenerationOutcome, GenerationUnsupported}
import s3j.macros.codegen.PluginUtils
import s3j.macros.generic.{GenerationConfidence, ImplicitBehavior}
import s3j.macros.modifiers.{Modifier, ModifierKey, ModifierParser, ModifierSet}

import scala.quoted.{Expr, Quotes, Type, quotes}

object MeowingRuntime {
  val meowingCodec: JsonFormat[String] =
    BasicFormats.stringFormat.mapFormat(
      encoding = _.replaceAll("\\s+", " meow "),
      decoding = _.replaceAll(" meow ", " ")
    )
}

class MeowingPlugin extends Plugin {
  def name: String = "Meowing plugin"

  private case object MeowingIdentity

  override def modifierParser(using c: PluginContext): ModifierParser = ModifierParser.builder
    .parse[meowingString](MeowingModifier)
    .build()

  private def matches[T](modifiers: ModifierSet)(using Quotes, Type[T]): Boolean =
    PluginUtils.typeEquals[T, String] && modifiers.contains(MeowingModifier.key)

  override def implicitBehavior[T](modifiers: ModifierSet)(using Quotes, PluginContext, Type[T]): ImplicitBehavior =
    if (matches[T](modifiers)) ImplicitBehavior.Suppress
    else ImplicitBehavior.Neutral

  override def generate[T](modifiers: ModifierSet)(using Quotes, GenerationContext, Type[T]): GenerationOutcome = {
    import quotes.reflect.*
    if (!matches[T](modifiers)) GenerationUnsupported
    else new GenerationCandidate {
      def confidence: GenerationConfidence = GenerationConfidence.Certain
      def identity: AnyRef = MeowingIdentity
      def generate(using Quotes)(): Expr[Any] = '{ MeowingRuntime.meowingCodec }
      override def simpleGeneration: Boolean = true
    }
  }
}

/** When applied to a field, causes string to be serialized in meowing way */
@usePlugin[MeowingPlugin]
class meowingString extends StaticAnnotation

case object MeowingModifier extends Modifier {
  val key: ModifierKey[_ <: MeowingModifier.this.type] = ModifierKey("meowingString")
}
