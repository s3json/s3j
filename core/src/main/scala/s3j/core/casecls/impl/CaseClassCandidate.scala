package s3j.core.casecls.impl

import s3j.core.casecls.CaseClassContext.{DecodingCode, DecodingEnvironment, GenerationEnvironment, ObjectField}
import s3j.core.casecls.CaseClassUtils
import s3j.core.casecls.modifiers.UnknownKeysModifier
import s3j.format.util.{DecoderUtils, ObjectFormatUtils}
import s3j.format.{JsonDecoder, JsonEncoder, JsonFormat, util}
import s3j.io.{JsonReader, JsonToken, JsonWriter, KeyHandle}
import s3j.macros.{CodecExpr, GenerationContext}
import s3j.macros.GenerationContext.GenerationCandidate
import s3j.macros.codegen.{CodeUtils, Variable}
import s3j.macros.generic.{GenerationConfidence, GenerationMode}
import s3j.macros.modifiers.ModifierSet
import s3j.macros.schema.SchemaExpr

import scala.quoted.{Expr, Quotes, Type, quotes}

private[casecls] class CaseClassCandidate[T](modifiers: ModifierSet)(using c: GenerationContext)(using Quotes, Type[T]) {
  import quotes.reflect.*

  private val allowUnknownKeys = modifiers.get(UnknownKeysModifier).fold(false)(_.allow)
  private val obj = new CaseClassObjectBuilder[T](
    CaseClassObjectBuilder.StackEntry(
      t = Type.of[T],
      modifiers = modifiers,
      path = Nil,
      label = None
    ) :: Nil
  )

  private val field: ObjectField[T] =
    if (allowUnknownKeys) CaseClassUtils.allowUnknownKeys(obj.result)
    else obj.result

  val candidate: GenerationCandidate =
    new GenerationCandidate {
      def confidence: GenerationConfidence = 500
      def identity: AnyRef = field.identity
      def generate(using q: Quotes)(): CodecExpr[?] = CaseClassUtils.generateCode(field).format(c.generationMode)
      def generateSchema(using Quotes)(): SchemaExpr[?] = CaseClassUtils.generateSchema[T](field)
    }
}
