package s3j.core.binary

import s3j.annotations.{base64, base64url, hex, hexUpper}
import s3j.format.impl.{BinaryEncoding, BinaryFormats}
import s3j.macros.GenerationContext.{GenerationCandidate, GenerationOutcome, GenerationRejection, GenerationUnsupported}
import s3j.macros.generic.{GenerationConfidence, ImplicitBehavior}
import s3j.macros.modifiers.{ModifierParser, ModifierSet}
import s3j.macros.schema.SchemaExpr
import s3j.macros.{GenerationContext, Plugin, PluginContext}

import scala.quoted.runtime.impl.TypeImpl
import scala.quoted.{Expr, Quotes, Type, quotes}

class BinaryPlugin extends Plugin {
  def name: String = "Binary plguin"

  /** @return Modifier parser for this plugin */
  override def modifierParser(using PluginContext): ModifierParser = ModifierParser.builder
    .parse[base64](BinaryFormatModifier(BinaryFormat.Base64))
    .parse[base64url](BinaryFormatModifier(BinaryFormat.Base64Url))
    .parse[hex](BinaryFormatModifier(BinaryFormat.HexLowercase))
    .parse[hexUpper](BinaryFormatModifier(BinaryFormat.HexUppercase))
    .build()

  private def generateBinary[T](using Quotes, Type[T]): Option[Expr[BinaryEncoding] => Expr[Any]] = {
    import quotes.reflect.*
    val t = TypeRepr.of[T]
    if (t =:= TypeRepr.of[Array[Byte]]) Some(enc => '{ new BinaryFormats.ByteArrayFormat($enc) })
    else None
  }

  override def implicitBehavior[T](modifiers: ModifierSet)(using Quotes, PluginContext, Type[T]): ImplicitBehavior =
    if (modifiers.contains(BinaryFormatModifier) && generateBinary[T].isDefined) ImplicitBehavior.Suppress
    else ImplicitBehavior.Neutral

  override def generate[T](modifiers: ModifierSet)(using Quotes, GenerationContext, Type[T]): GenerationOutcome =
    generateBinary[T] match {
      case Some(fn) =>
        if (!modifiers.contains(BinaryFormatModifier))
          GenerationRejection("No binary encoding format is set. Use annotations like @base64 to select one.")
        else new GenerationCandidate {
          def confidence: GenerationConfidence = 1000
          def identity: AnyRef = modifiers(BinaryFormatModifier)
          override def simpleGeneration: Boolean = true

          def generate(using Quotes)(): Expr[Any] = {
            val fmt = modifiers(BinaryFormatModifier).format
            fn(fmt.fn)
          }

          def generateSchema(using Quotes)(): SchemaExpr[Any] = ???
        }

      case None => GenerationUnsupported
    }
}
