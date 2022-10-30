package s3j.internal.cclass

import s3j.ast.JsObject
import s3j.format.util.ObjectFormatUtils
import s3j.format.util.ObjectFormatUtils.RestFieldsBuilder
import s3j.internal.cclass.CaseClassModel.DecodingEnv
import s3j.internal.utils.{GenerationMode, NestedFormat, Variable}
import s3j.io.{JsonReader, JsonWriter, KeyHandle}

import scala.quoted.{Expr, Quotes, Type, quotes}

class CaseClassModelImpl[T](mode: GenerationMode)(using q: Quotes, tt: Type[T])
extends CaseClassModel[T] {
  import q.reflect.*

  private val typeRepr: TypeRepr = TypeRepr.of[T]
  private val typeSymbol: Symbol = typeRepr.typeSymbol

  private enum FieldType {
    case Regular
    case Optional
    case Inline
    case RestFields
  }

  private def annotationType(t: Tree): String =
    t match {
      case Apply(Select(New(tpe), _), _) => tpe.tpe.typeSymbol.fullName
      case _ => ""
    }

  private class CaseField(val ctorSymbol: Symbol) {
    type R

    val fieldSymbol: Symbol = typeSymbol.fieldMember(ctorSymbol.name)
    val key: String = fieldSymbol.name

    val tpe: TypeRepr = typeRepr.memberType(fieldSymbol)
    given rTypeGiven: Type[R] = tpe.asType.asInstanceOf[Type[R]]

    val fieldType: FieldType = tpe match {
      case AppliedType(base, List(_)) if base =:= TypeRepr.of[Option] => FieldType.Optional
      case _ =>
        val isInline = ctorSymbol.annotations.exists(a => annotationType(a) == "s3j.annotations.inlineObject")

        if (fieldSymbol.name == "restFields") FieldType.RestFields
        else if (isInline) FieldType.Inline
        else FieldType.Regular
    }

    override def toString: String = s"CaseField(sym=${fieldSymbol.name}, tpe=${tpe.show}, fieldType=$fieldType)"
  }

  private class RegularField(val f: CaseField) extends CaseClassModel[f.R] {
    val keys: Set[String] = Set(f.key)
    val dynamicKeys: Boolean = false

    val nested: NestedFormat[f.R] = NestedFormat.lookup(mode)

    def encode(writer: Expr[JsonWriter], value: Expr[f.R])(using Quotes): Expr[Any] = '{
      $writer.key(${ Expr(f.key) })
      ${nested.encoder}.encode($writer, $value)
    }

    def decode(reader: Expr[JsonReader])(using Quotes, DecodingEnv): CaseClassModel.DecoderCode[f.R] =
      new CaseClassModel.DecoderCode[f.R] {
        private val result: Variable[f.R] = Variable.create(f.ctorSymbol.name)
        def variables: Seq[Variable[_]] = Seq(result)

        def staticKey(key: String)(using Quotes): Expr[Any] =
          result := '{ ${nested.decoder}.decode($reader) }

        def dynamicKey(key: Expr[KeyHandle])(using Quotes): Expr[Any] =
          throw new UnsupportedOperationException("RegularFIeld.dynamicKey")

        def finalCode()(using Quotes): Expr[Any] = {
          val present = implicitly[DecodingEnv].isKeyPresent(f.key)
          '{ if (!$present) ObjectFormatUtils.throwMissingKey($reader, ${ Expr(f.key) }) }
        }

        def result()(using Quotes): Expr[f.R] = result.value
      }
  }

  private class OptionalField(val f: CaseField) extends CaseClassModel[f.R] {
    type T
    given tType: Type[T] = TypeRepr.of[f.R].typeArgs.head.asType.asInstanceOf[Type[T]]

    val nested: NestedFormat[T] = NestedFormat.lookup(mode)

    val keys: Set[String] = Set(f.key)
    val dynamicKeys: Boolean = false

    def encode(writer: Expr[JsonWriter], value: Expr[f.R])(using Quotes): Expr[Any] = {
      val valueOpt = value.asExprOf[Option[T]]
      '{ if ($valueOpt.isDefined) { $writer.key(${ Expr(f.key) }); ${nested.encoder}.encode($writer, $valueOpt.get) } }
    }

    def decode(reader: Expr[JsonReader])(using Quotes, DecodingEnv): CaseClassModel.DecoderCode[f.R] =
      new CaseClassModel.DecoderCode[f.R] {
        private val result: Variable[Option[T]] = Variable.create(f.ctorSymbol.name)('{ None })

        def variables: Seq[Variable[_]] = Seq(result)

        def staticKey(key: String)(using Quotes): Expr[Any] =
          result := '{ Some(${nested.decoder}.decode($reader)) }

        def dynamicKey(key: Expr[KeyHandle])(using Quotes): Expr[Any] =
          throw new UnsupportedOperationException("OptionalField.dynamicKey")

        def finalCode()(using Quotes): Expr[Any] = '{}
        def result()(using Quotes): Expr[f.R] = result.value.asExprOf[f.R]
      }
  }

  private class RestFieldsField(val f: CaseField) extends CaseClassModel[f.R] {
    val keys: Set[String] = Set.empty
    val dynamicKeys: Boolean = true

    def encode(writer: Expr[JsonWriter], value: Expr[f.R])(using Quotes): Expr[Any] =
      '{ ObjectFormatUtils.writeRestFields($writer, ${ value.asExprOf[JsObject] }) }

    def decode(reader: Expr[JsonReader])(using Quotes, DecodingEnv): CaseClassModel.DecoderCode[f.R] =
      new CaseClassModel.DecoderCode[f.R] {
        private val result: Variable[RestFieldsBuilder] = Variable.create(f.ctorSymbol.name)('{ new RestFieldsBuilder })

        def variables: Seq[Variable[_]] = Seq(result)

        def staticKey(key: String)(using Quotes): Expr[Any] =
          throw new UnsupportedOperationException("RestFieldsField.staticKey")

        def dynamicKey(key: Expr[KeyHandle])(using Quotes): Expr[Any] =
          '{ ${result.value}.readField($key.toString, $reader) }

        def finalCode()(using Quotes): Expr[Any] = '{}
        def result()(using Quotes): Expr[f.R] = '{ ${result.value}.result() }.asExprOf[f.R]
      }
  }

  private val fields: Seq[CaseField] = typeSymbol.primaryConstructor.paramSymss.head.map(s => new CaseField(s))
  private val fieldModels: Seq[CaseClassModel[?]] = fields.map { f =>
    f.fieldType match {
      case FieldType.Regular    => new RegularField(f)
      case FieldType.Optional   => new OptionalField(f)
      case FieldType.Inline     => new CaseClassModelImpl[f.R](mode)
      case FieldType.RestFields => new RestFieldsField(f)
    }
  }

  val keys: Set[String] = fieldModels.flatMap(_.keys).toSet
  val dynamicKeys: Boolean = fieldModels.exists(_.dynamicKeys)

  def encode(writer: Expr[JsonWriter], value: Expr[T])(using Quotes): Expr[Any] = {
    val encoders = Vector.newBuilder[Expr[Any]]

    for ((f, m) <- fields.zip(fieldModels)) {
      val selected = Select(value.asTerm, f.fieldSymbol).asExpr
      encoders += m.asInstanceOf[CaseClassModel[Any]].encode(writer, selected)
    }

    Expr.block(encoders.result().toList, '{ () })
  }

  def decode(reader: Expr[JsonReader])(using Quotes, DecodingEnv): CaseClassModel.DecoderCode[T] =
    new CaseClassModel.DecoderCode[T] {
      private val _decoders: Seq[CaseClassModel.DecoderCode[?]] = fieldModels.map(_.decode(reader))
      private val _keys: Map[String, CaseClassModel.DecoderCode[?]] =
        fieldModels.zip(_decoders)
          .flatMap { case (f, d) => f.keys.map(_ -> d) }
          .toMap

      private val _dynamicKey: Option[CaseClassModel.DecoderCode[?]] =
        fieldModels.zip(_decoders).find(_._1.dynamicKeys).map(_._2)

      def variables: Seq[Variable[_]] = _decoders.flatMap(_.variables)

      def staticKey(key: String)(using Quotes): Expr[Any] =
        _keys(key).staticKey(key)

      def dynamicKey(key: Expr[KeyHandle])(using Quotes): Expr[Any] =
        _dynamicKey.get.dynamicKey(key)

      def finalCode()(using Quotes): Expr[Any] =
        Expr.block(_decoders.map(_.finalCode()).toList, '{ () })

      def result()(using Quotes): Expr[T] = {
        import quotes.reflect.*

        Apply(
          Select(New(TypeTree.of[T]), TypeRepr.of[T].typeSymbol.primaryConstructor),
          _decoders.map(_.result().asTerm).toList
        ).asExprOf[T]
      }
    }
}
