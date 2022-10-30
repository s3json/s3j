package s3j.core.casecls.impl

import s3j.core.casecls.CaseClassContext
import s3j.core.casecls.CaseClassContext.{DecodingCode, DecodingEnvironment, GenerationEnvironment, ObjectField, SchemaCode}
import s3j.format.util.DecoderUtils
import s3j.io.{JsonReader, JsonWriter}
import s3j.macros.codegen.Variable
import s3j.macros.schema.SchemaExpr
import s3j.schema.model.SchemaDocument

import scala.quoted.{Expr, Quotes}

object ObjectFields {
  private case class AllowUnknownKeysIdentity(underlying: Any)

  /** Field wrapper which skips unknown fields */
  class AllowUnknownKeysField[T](base: ObjectField[T]) extends ObjectField[T] {
    def identity: AnyRef = AllowUnknownKeysIdentity(base.identity)
    def handledKeys: Set[String] = base.handledKeys
    def handlesDynamicKeys: Boolean = true

    def generateEncoder(writer: Expr[JsonWriter], value: Expr[T])(using Quotes, GenerationEnvironment): Expr[Any] =
      base.generateEncoder(writer, value)

    def generateDecoder(using Quotes, DecodingEnvironment): CaseClassContext.DecodingCode = {
      val baseDecoder = base.generateDecoder
      new DecodingCode {
        def usedVariables: Seq[Variable[_]] = baseDecoder.usedVariables

        def decodeKey(key: String, reader: Expr[JsonReader])(using Quotes): Expr[Any] =
          baseDecoder.decodeKey(key, reader)

        override def decodeDynamicKey(key: Expr[String], reader: Expr[JsonReader])(using Quotes): Expr[Any] =
          '{ DecoderUtils.skipValue($reader) }

        def decodeFinal()(using Quotes): Expr[Any] = baseDecoder.decodeFinal()
        def decodeResult()(using Quotes): Expr[Any] = baseDecoder.decodeResult()
      }
    }

    def generateSchema(using Quotes): CaseClassContext.SchemaCode = {
      val baseSchema = base.generateSchema
      new SchemaCode {
        def requiredKeys: Set[String] = baseSchema.requiredKeys
        def keyOrdering: Seq[String] = baseSchema.keyOrdering
        def key(key: String): SchemaExpr[?] = baseSchema.key(key)
        def dynamicKey: Option[SchemaExpr[?]] = None
        def dynamicKeyNames: Option[SchemaExpr[String]] = None
      }
    }
  }
}
