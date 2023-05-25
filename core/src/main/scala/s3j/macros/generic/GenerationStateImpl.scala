package s3j.macros.generic

import s3j.format.{JsonDecoder, JsonEncoder}
import s3j.macros.{CodecExpr, GenerationContext}
import s3j.macros.modifiers.ModifierSet
import s3j.macros.schema.SchemaExpr
import s3j.macros.traits.GenerationResult

import java.lang.reflect.InvocationTargetException
import scala.annotation.threadUnsafe
import scala.collection.mutable
import scala.quoted.{Expr, Quotes, Type}
import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import dotty.tools.dotc.core.{Symbols, Types, Flags as DFlags}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Decorators.*
import s3j.io.{JsonReader, JsonWriter}
import s3j.macros.FreshPluginContext.StackHandle
import s3j.schema.JsonSchema

private[macros] trait GenerationStateImpl { this: PluginContextImpl =>
  import q.reflect.*

  private[macros] class GenerationResultImpl[T](handle: SerializerHandle)(using Type[T]) extends GenerationResult[T] {
    def raw(using q: Quotes): Expr[Any] =
      handle.reference

    private def checkEncoders(): Unit = {
      if (!handle.mode.generateEncoders) {
        throw new IllegalStateException("Encoders are not generated in mode " + handle.mode)
      }
    }

    private def checkDecoders(): Unit = {
      if (!handle.mode.generateDecoders) {
        throw new IllegalStateException("Decoders are not generated in mode " + handle.mode)
      }
    }

    def encoder(using Quotes): Expr[JsonEncoder[T]] = {
      checkEncoders()
      raw.asExprOf[JsonEncoder[T]]
    }

    def decoder(using Quotes): Expr[JsonDecoder[T]] = {
      checkDecoders()
      raw.asExprOf[JsonDecoder[T]]
    }

    def encode(writer: Expr[JsonWriter], value: Expr[T])(using Quotes): Expr[Unit] = {
      checkEncoders()
      if (handle.codec.exists(_.inlineEncode)) {
        handle.codec.get.asInstanceOf[CodecExpr[T]].encode(writer, value)
      } else {
        '{ $encoder.encode($writer, $value) }
      }
    }

    def decode(reader: Expr[JsonReader])(using Quotes): Expr[T] = {
      checkDecoders()
      if (handle.codec.exists(_.inlineDecode)) {
        handle.codec.get.asInstanceOf[CodecExpr[T]].decode(reader)
      } else {
        '{ $decoder.decode($reader) }
      }
    }

    def schema: SchemaExpr[T] = {
      if (!handle.mode.schema) {
        throw new IllegalStateException("Schema is not generated in mode " + handle.mode)
      }

      if (handle.schema.isDefined) {
        handle.schema.get.asInstanceOf[SchemaExpr[T]]
      } else {
        SchemaExpr.fromExpr(raw.asExprOf[JsonSchema[T]])
      }
    }
  }

  // Caching of generated serializers is absolutely necessary:
  // 1) for performance reasons to avoid generating same serializers from scratch
  // 2) for code bloat reasons to reuse equivalent serializers
  // 3) to handle recursive types

  // Caching consists of two layers:
  // 1) core layer comparing type and it's modifiers - cheap layer that could be used right from start
  // 2) plugin layer comparing some plugin-specific ('semantical') identities. These identites could be costly to
  //    generate (they could require parsing of everything just as during generation), but they could lead to much more
  //    precise cache hits

  protected class SerializerHandle(val mode: GenerationMode, val targetType: TypeRepr, val identity: AnyRef) {
    type A // Applied
    type T // Target

    given typeApplied: Type[A] = mode.appliedType(targetType).asType.asInstanceOf[Type[A]]
    given typeTarget: Type[T] = targetType.asType.asInstanceOf[Type[T]]

    private val stackHandle: StackHandle[A] = _stack.addEntry(targetType.typeSymbol.name)
    def nestedQuotes: Quotes = stackHandle.nestedQuotes
    def reference: Expr[A] = stackHandle.reference

    var codec: Option[CodecExpr[T]] = None     // N.B. Don't use codec.raw for any expressions!
    var schema: Option[SchemaExpr[T]] = None

    // Schemas use delayed initialization to have most precise type, including possible inline schemas
    // Such precise type isn't available until schema is actually generated, so variable isn't created right away.
    // But for recursive schemas we will hit `reference` method before generation is finished - in this case
    // type is initialized as generic `JsonSchema[T]` - recursive schemas can't be inlined anyway.

    def setCodec(codec: CodecExpr[T]): Unit = {
      this.codec = Some(codec)
      stackHandle.setDefinition(codec.raw.asExprOf[A])
    }

    def setSchema(schema: SchemaExpr[T]): Unit = {
      this.schema = Some(schema)
      stackHandle.setDefinition(SchemaExpr.toExpr(schema).asExprOf[A])
    }

    def setSchema(schema: Expr[JsonSchema[T]]): Unit = {
      this.schema = Some(SchemaExpr.fromExpr(schema))
      stackHandle.setDefinition(schema.asExprOf[A])
    }

    def toNestedResult[T](using Type[T]): GenerationResult[T] = new GenerationResultImpl[T](this)
  }

  protected given typeIdentityConv: Conversion[TypeRepr, TypeIdentity] = new TypeIdentity(_)

  // TypeRepr's should be compared with "=:=", not with "=="
  protected final class TypeIdentity(val x: TypeRepr) {
    private[this] val _hash: Int = Type.show(using x.asType).hashCode

    override def equals(obj: Any): Boolean = obj match {
      case t: TypeIdentity => x =:= t.x
      case _ => false
    }

    override def hashCode: Int = _hash
    override def toString: String = Type.show(using x.asType)
  }

  protected sealed trait CachingKey
  protected case class BasicCachingKey(target: TypeIdentity, modifiers: ModifierSet) extends CachingKey
  protected case class ImplicitCachingKey(target: TypeIdentity) extends CachingKey
  protected case class PluginCachingKey(target: TypeIdentity, plugin: String, identity: AnyRef) extends CachingKey

  protected val _serializers: mutable.Map[CachingKey, SerializerHandle] = mutable.HashMap.empty
  protected var _serializerSet: mutable.Set[SerializerHandle] = mutable.HashSet.empty
}
