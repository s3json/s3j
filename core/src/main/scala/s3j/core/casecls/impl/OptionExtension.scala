package s3j.core.casecls.impl

import s3j.core.casecls.{CaseClassContext, CaseClassExtension}
import s3j.core.casecls.CaseClassContext.*
import s3j.core.casecls.modifiers.NullOptionModifier
import s3j.format.util.DecoderUtils
import s3j.io.{JsonReader, JsonToken, JsonWriter}
import s3j.macros.codegen.Variable
import s3j.macros.generic.GenerationConfidence
import s3j.macros.schema.SchemaExpr
import s3j.macros.traits.GenerationResult

import scala.quoted.*

class OptionExtension extends CaseClassExtension {
  private case class OptionIdentity(nullOption: Boolean)

  private class ObjectFieldImpl[T](using c: CaseClassContext)(field: FieldRequest[Option[T]])
                                  (using Quotes, Type[T])
  extends ObjectField[Option[T]] {
    val nullOption: Boolean = field.fieldModifiers.contains(NullOptionModifier.key)
    lazy val nested: GenerationResult[T] = c.nested[T]
      .modifiers(field.ownModifiers)
      .build()

    def identity: AnyRef = OptionIdentity(nullOption)
    def handledKeys: Set[String] = Set(field.key)
    def handlesDynamicKeys: Boolean = false

    def generateEncoder(writer: Expr[JsonWriter], value: Expr[Option[T]])
                       (using Quotes, GenerationEnvironment): Expr[Any] = 
    {
      val keyExpr = Expr(field.key)
      
      if (nullOption) '{
        $writer.key($keyExpr)
        if ($value.isDefined) ${ nested.encode(writer, '{ $value.get }) }
        else $writer.nullValue()
      } else '{ 
        if ($value.isDefined) { 
          $writer.key($keyExpr)
          ${ nested.encode(writer, '{ $value.get }) } 
        } 
      }
    }

    def generateDecoder(using q: Quotes, env: DecodingEnvironment): DecodingCode =
      new DecodingCode {
        private val result: Variable[Option[T]] = Variable.create(field.key)('{ None })
        def usedVariables: Seq[Variable[_]] = Seq(result)

        def decodeKey(key: String, reader: Expr[JsonReader])(using Quotes): Expr[Any] =
          result := '{ DecoderUtils.decodeOption($reader, ${nested.decoder}) }

        def decodeFinal()(using Quotes): Expr[Any] = '{}

        def decodeResult()(using Quotes): Expr[Any] = result.value
      }

    def generateSchema(using Quotes): SchemaCode[Option[T]] =
      new SchemaCode[Option[T]] {
        def keyOrdering: Seq[String] = Seq(field.key)
        def requiredKeys: Set[String] = Set.empty

        def key(key: String): SchemaExpr[_] =
          SchemaExpr.makeOptional(nested.schema)

        def dynamicKey: Option[SchemaExpr[_]] = None
        def dynamicKeyNames: Option[SchemaExpr[String]] = None
      }
  }

  override def processField[T](using CaseClassContext)(field: FieldRequest[T])
                              (using q: Quotes, t: Type[T]): GenerationOutcome[T] =
  {
    import q.reflect.*
    if (!(TypeRepr.of[T] <:< TypeRepr.of[Option[Any]])) GenerationUnsupported
    else new GenerationCandidate[T] {
      def confidence: GenerationConfidence = 1000
      def result: ObjectField[T] = {
        type I
        given inner: Type[I] = TypeRepr.of[T].typeArgs.head.asType.asInstanceOf[Type[I]]
        new ObjectFieldImpl[I](field.asInstanceOf[FieldRequest[Option[I]]]).asInstanceOf[ObjectField[T]]
      }
    }
  }
}
