package s3j.core.numbers

import s3j.annotations.jsonUnsigned
import s3j.format.impl.NumberFormats
import s3j.macros.GenerationContext.{GenerationCandidate, GenerationOutcome, GenerationRejection, GenerationUnsupported}
import s3j.macros.modifiers.{ModifierParser, ModifierSet}
import s3j.macros.{GenerationContext, Plugin, PluginContext}

import scala.quoted.{Expr, Quotes, Type}

class NumbersPlugin extends Plugin {
  def name: String = "Numbers plugin"

  private case object UnsignedNumberIdentity

  override def modifierParser(using PluginContext): ModifierParser = ModifierParser.builder
    .parse[jsonUnsigned](UnsignedModifier)
    .build()

  private def makeCandidate(expr: Quotes ?=> Expr[Any]): GenerationCandidate =
    new GenerationCandidate {
      val confidence: Option[Int] = None
      val identity: AnyRef = UnsignedNumberIdentity
      def generate(using Quotes)(): Expr[Any] = expr
      override def simpleGeneration: Boolean = true
    }

  override def generate[T](modifiers: ModifierSet)(using Quotes, GenerationContext, Type[T]): GenerationOutcome =
    if (!modifiers.contains(UnsignedModifier.key)) GenerationUnsupported
    else Type.of[T] match {
      case '[ Byte ] => makeCandidate('{ NumberFormats.unsignedByteFormat })
      case '[ Short ] => makeCandidate('{ NumberFormats.unsignedShortFormat })
      case '[ Int ] => makeCandidate('{ NumberFormats.unsignedIntFormat })
      case '[ Long ] => makeCandidate('{ NumberFormats.unsignedLongFormat })
      case _ => GenerationRejection("no unsigned type is available for " + Type.show[T])
    }
}
