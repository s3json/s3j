package s3j.core.casecls.impl

import s3j.core.casecls.CaseClassContext.*
import s3j.core.casecls.modifiers.{FieldCaseModifier, InlineObjectModifier, KeyPrefixModifier}
import s3j.core.casecls.{CaseClassContext, CaseClassExtension}
import s3j.io.{JsonReader, JsonWriter}
import s3j.macros.codegen.Variable
import s3j.macros.generic.{CaseConvention, GenerationConfidence}
import s3j.macros.schema.SchemaExpr

import scala.quoted.{Expr, Quotes, Type}

private[casecls] class InlineObjectExtension extends CaseClassExtension {
  private case object InlineObjectIdentity

  private class ObjectFieldImpl[T](field: FieldRequest[T])(using q: Quotes, c: CaseClassContext, t: Type[T])
  extends ObjectField[T] {
    val nestedKeyPrefix: Option[KeyPrefixModifier] = field.fieldModifiers
      .get(KeyPrefixModifier)
      .map {
        case m if m.prefix.isEmpty => KeyPrefixModifier(field.key)
        case other => other
      }

    val nested: ObjectField[T] = c.objectModel[T]
      .modifiers(field.fieldModifiers -- Seq(InlineObjectModifier, KeyPrefixModifier) ++ nestedKeyPrefix)
      .build()

    def identity: AnyRef = InlineObjectIdentity
    def handledKeys: Set[String] = nested.handledKeys
    def handlesDynamicKeys: Boolean = nested.handlesDynamicKeys

    def generateEncoder(writer: Expr[JsonWriter], value: Expr[T])(using Quotes, GenerationEnvironment): Expr[Any] =
      nested.generateEncoder(writer, value)

    def generateDecoder(using Quotes, DecodingEnvironment): DecodingCode = nested.generateDecoder
    def generateSchema(using Quotes): CaseClassContext.SchemaCode[T] = nested.generateSchema
  }

  override def processField[T](using CaseClassContext)(field: FieldRequest[T])(using Quotes, Type[T]): GenerationOutcome[T] = {
    if (!field.fieldModifiers.contains(InlineObjectModifier)) GenerationUnsupported
    else new GenerationCandidate[T] {
      def confidence: GenerationConfidence = GenerationConfidence.Certain
      def result: ObjectField[T] = new ObjectFieldImpl[T](field)
    }
  }
}
