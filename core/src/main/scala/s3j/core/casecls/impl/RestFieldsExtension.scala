package s3j.core.casecls.impl

import s3j.ast.JsObject
import s3j.core.casecls.CaseClassContext.*
import s3j.core.casecls.modifiers.RestFieldsModifier
import s3j.core.casecls.{CaseClassContext, CaseClassExtension}
import s3j.format.util.ObjectFormatUtils
import s3j.format.util.ObjectFormatUtils.RestFieldsBuilder
import s3j.io.{JsonReader, JsonWriter}
import s3j.macros.codegen.Variable

import scala.quoted.{Expr, Type, Quotes, quotes}

class RestFieldsExtension extends CaseClassExtension {
  private case object RestFieldsIdentity

  private class ObjectFieldImpl[T](using CaseClassContext, Quotes, Type[T]) extends ObjectField[T] {
    def identity: AnyRef = RestFieldsIdentity
    def handledKeys: Set[String] = Set.empty
    def handlesDynamicKeys: Boolean = true

    def generateEncoder(writer: Expr[JsonWriter], value: Expr[T])(using Quotes, GenerationEnvironment): Expr[Any] =
      '{ ObjectFormatUtils.writeRestFields($writer, ${value.asExprOf[JsObject]}) }

    def generateDecoder(using Quotes, DecodingEnvironment): DecodingCode =
      new DecodingCode {
        private val builder: Variable[RestFieldsBuilder] =
          Variable.createConst("restFields")('{ new RestFieldsBuilder })

        def usedVariables: Seq[Variable[_]] = Seq(builder)

        def decodeKey(key: String, reader: Expr[JsonReader])(using Quotes): Expr[Any] =
          throw new UnsupportedOperationException("RestFields.decodeKey")

        override def decodeDynamicKey(key: Expr[String], reader: Expr[JsonReader])(using Quotes): Expr[Any] =
          '{ $builder.readField($key, $reader) }

        def decodeFinal()(using Quotes): Expr[Any] = '{}
        def decodeResult()(using Quotes): Expr[Any] = '{ $builder.result() }
      }
  }

  override def processField[T](using c: CaseClassContext)(field: FieldRequest[T])
                              (using Quotes, Type[T]): GenerationOutcome[T] =
  {
    if (!field.fieldModifiers.contains(RestFieldsModifier.key)) GenerationUnsupported
    else {
      import quotes.reflect.*
      if (!(TypeRepr.of[T] =:= TypeRepr.of[JsObject])) {
        c.report.errorAndAbort("@restFields field must have type s3j.ast.JsObject")
      }

      new GenerationCandidate[T] {
        def confidence: Option[Int] = None
        def result: ObjectField[T] = new ObjectFieldImpl[T]()
      }
    }
  }
}
