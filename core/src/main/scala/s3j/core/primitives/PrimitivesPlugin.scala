package s3j.core.primitives

import s3j.annotations.jsonUnsigned
import s3j.core.primitives.PrimitiveSpecification.FormatSpecification
import s3j.format.impl.NumberFormats
import s3j.macros.GenerationContext.{GenerationCandidate, GenerationOutcome, GenerationRejection, GenerationUnsupported}
import s3j.macros.generic.{GenerationConfidence, ImplicitBehavior}
import s3j.macros.modifiers.{ModifierParser, ModifierSet}
import s3j.macros.schema.SchemaExpr
import s3j.macros.{CodecExpr, GenerationContext, Plugin, PluginContext}
import s3j.schema.{BasicSchemas, JsonSchema}

import scala.quoted.{Expr, Quotes, Type, quotes}

private[primitives] object PrimitivesPlugin {
  private case class PrimitiveIdentity(unsigned: Boolean)
}

class PrimitivesPlugin extends Plugin {
  import PrimitivesPlugin.*

  def name: String = "Primitives plugin"

  private class CandidateImpl[T](spec: PrimitiveSpecification[T], modifiers: ModifierSet)
                                (using tt: Type[T], ctx: GenerationContext)
  extends GenerationCandidate {
    val unsigned: Boolean = modifiers.contains(UnsignedModifier)
    def confidence: GenerationConfidence = GenerationConfidence.Certain
    def identity: AnyRef = PrimitiveIdentity(unsigned)

    private def format(using q: Quotes): FormatSpecification[T] = {
      import q.reflect.*
      if (unsigned) {
        spec
          .unsigned
          .getOrElse(ctx.report.errorAndAbort("Unsigned format is not supported for " + spec.typeName))
      } else spec.default
    }

    def generate(using Quotes)(): CodecExpr[_] = {
      val f = format
      CodecExpr.builder(f.format).encode(f.encode).decode(f.decode).build()
    }

    def generateSchema(using Quotes)(): SchemaExpr[_] =
      SchemaExpr.fromExpr(format.schema)
  }

  override def modifierParser(using PluginContext): ModifierParser = ModifierParser.builder
    .parse[jsonUnsigned](UnsignedModifier)
    .build()

  private def getPrimitive[T](using Quotes, Type[T]): Option[PrimitiveSpecification[T]] = {
    import quotes.reflect.*
    PrimitiveSpecification.All
      .get(TypeRepr.of[T].dealias.typeSymbol.fullName)
      .map(_.asInstanceOf[PrimitiveSpecification[T]])
  }

  override def implicitBehavior[T](modifiers: ModifierSet)(using Quotes, PluginContext, Type[T]): ImplicitBehavior = {
    if (getPrimitive[T].isDefined) ImplicitBehavior.Suppress
    else ImplicitBehavior.Neutral
  }

  override def generate[T](modifiers: ModifierSet)(using Quotes, GenerationContext, Type[T]): GenerationOutcome =
    getPrimitive[T] match {
      case Some(p) => new CandidateImpl[T](p, modifiers)
      case None => GenerationUnsupported
    }
}
