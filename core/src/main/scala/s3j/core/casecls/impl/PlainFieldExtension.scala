package s3j.core.casecls.impl

import s3j.core.casecls.CaseClassContext.*
import s3j.core.casecls.{CaseClassContext, CaseClassExtension}
import s3j.io.{JsonReader, JsonWriter}
import s3j.macros.codegen.Variable
import s3j.macros.generic.GenerationConfidence
import s3j.macros.schema.SchemaExpr
import s3j.macros.traits.GenerationResult

import scala.annotation.threadUnsafe
import scala.quoted.{Expr, Quotes, Type}

/** Extension which always generates single field with nested serializer */
private[casecls] class PlainFieldExtension extends CaseClassExtension {
  private case object PlainFieldIdentity

  private class ObjectFieldImpl[T](field: FieldRequest[T])(using q: Quotes, c: CaseClassContext, t: Type[T])
  extends ObjectField[T] {
    val identity: AnyRef = PlainFieldIdentity
    val handledKeys: Set[String] = Set(field.key)
    val handlesDynamicKeys: Boolean = false

    lazy val nested: GenerationResult[T] = c
      .nested[T]
      .modifiers(field.ownModifiers)
      .build()

    def generateEncoder(writer: Expr[JsonWriter], value: Expr[T])
                       (using Quotes, GenerationEnvironment): Expr[Any] =
      '{ $writer.key(${ Expr(field.key) }); ${ nested.encode(writer, value) } }

    def generateDecoder(using q: Quotes, env: DecodingEnvironment): DecodingCode =
      new DecodingCode {
        private val result: Variable[T] = Variable.create(field.key)
        val usedVariables: Seq[Variable[_]] = Seq(result)

        def decodeKey(key: String, reader: Expr[JsonReader])(using Quotes): Expr[Any] =
          result := nested.decode(reader)

        def decodeFinal()(using Quotes): Expr[Any] = env.checkRequiredKey(field.key)
        def decodeResult()(using Quotes): Expr[Any] = result.value
      }

    /** @return Generated schemas for the field */
    def generateSchema(using Quotes): SchemaCode[T] =
      new SchemaCode {
        def requiredKeys: Set[String] = Set(field.key)
        def keyOrdering: Seq[String] = Seq(field.key)
        def key(key: String): SchemaExpr[?] = nested.schema
        def dynamicKey: Option[SchemaExpr[Any]] = None
        def dynamicKeyNames: Option[SchemaExpr[String]] = None
      }
  }

  override def processField[T](using CaseClassContext)(field: FieldRequest[T])
                              (using Quotes, Type[T]): GenerationOutcome[T] =
    new GenerationCandidate[T] {
      def confidence: GenerationConfidence = 0
      def result: ObjectField[T] = new ObjectFieldImpl[T](field)
    }
}
