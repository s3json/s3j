package s3j.core.numbers

import s3j.annotations.jsonUnsigned
import s3j.format.impl.NumberFormats
import s3j.macros.GenerationContext.{GenerationCandidate, GenerationOutcome, GenerationRejection, GenerationUnsupported}
import s3j.macros.generic.{GenerationConfidence, ImplicitBehavior}
import s3j.macros.modifiers.{ModifierParser, ModifierSet}
import s3j.macros.schema.SchemaExpr
import s3j.macros.{GenerationContext, Plugin, PluginContext}
import s3j.schema.{BasicSchemas, JsonSchema}

import scala.quoted.{Expr, Quotes, Type}

class NumbersPlugin extends Plugin {
  def name: String = "Numbers plugin"

  private case object UnsignedNumberIdentity

  override def modifierParser(using PluginContext): ModifierParser = ModifierParser.builder
    .parse[jsonUnsigned](UnsignedModifier)
    .build()

  private def makeCandidate[T](expr: Quotes ?=> Expr[Any], schema: Quotes ?=> Expr[JsonSchema[T]]): GenerationCandidate =
    new GenerationCandidate {
      val confidence: GenerationConfidence = GenerationConfidence.Certain
      val identity: AnyRef = UnsignedNumberIdentity
      def generate(using Quotes)(): Expr[Any] = expr
      def generateSchema(using Quotes)(): SchemaExpr[Any] = SchemaExpr.fromUntypedExpr(schema).asErased
      override def simpleGeneration: Boolean = true
    }

  override def implicitBehavior[T](modifiers: ModifierSet)(using Quotes, PluginContext, Type[T]): ImplicitBehavior =
    if (modifiers.contains(UnsignedModifier)) ImplicitBehavior.Suppress
    else ImplicitBehavior.Neutral

  override def generate[T](modifiers: ModifierSet)(using Quotes, GenerationContext, Type[T]): GenerationOutcome =
    if (!modifiers.contains(UnsignedModifier)) GenerationUnsupported
    else Type.of[T] match {
      case '[ Byte ] => makeCandidate('{ NumberFormats.unsignedByteFormat }, '{ BasicSchemas.unsignedByteSchema })
      case '[ Short ] => makeCandidate('{ NumberFormats.unsignedShortFormat }, '{ BasicSchemas.unsignedShortSchema })
      case '[ Int ] => makeCandidate('{ NumberFormats.unsignedIntFormat }, '{ BasicSchemas.unsignedIntSchema })
      case '[ Long ] => makeCandidate('{ NumberFormats.unsignedLongFormat }, '{ BasicSchemas.unsignedLongSchema })
      case _ => GenerationRejection("no unsigned type is available for " + Type.show[T])
    }
}
