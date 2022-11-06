package s3j.macros

import s3j.io.{JsonReader, JsonWriter}

import scala.collection.mutable
import scala.quoted.{Quotes, Expr}

object CodecExpr {
  /** Builder for convenient construction of [[CodecExpr]]'s. */
  trait Builder[T] {
    /** Provide a function for inline encode */
    def encode(f: Quotes ?=> (Expr[JsonWriter], Expr[T]) => Expr[Unit]): this.type

    /** Provide a function for inline decode */
    def decode(f: Quotes ?=> Expr[JsonReader] => Expr[T]): this.type

    /** @return CodecExpr instance */
    def build(): CodecExpr[T]
  }

  /** Wrapper for simple codec expressions without any inline methods */
  given exprWrapper[T]: Conversion[Expr[Any], CodecExpr[T]] = raw => builder(raw).build()

  /** @return A new builder instance for CodecExpr */
  def builder[T](raw: Expr[Any]): CodecExpr.Builder[T] = new BuilderImpl[T](raw)
}

trait CodecExpr[T] {
  /** @return Raw codec expression. Typed as `Any` as we don't know exact applied type */
  def raw: Expr[Any]

  /**
   * @return Whether encoding should be handled via [[encode]] method instead of just using the codec object.
   *         This is only an optimization, not a strict requirement - codec object should still be functional.
   */
  def inlineEncode: Boolean

  /**
   * @return Whether decoding should be handled via [[decode]] method instead of just using the codec object.
   *         This is only an optimization, not a strict requirement - codec object should still be functional.
   */
  def inlineDecode: Boolean

  /** @return Expression to encode a value. Called only when encoders are generated and [[inlineEncode]] is set. */
  def encode(writer: Expr[JsonWriter], value: Expr[T])(using Quotes): Expr[Unit]

  /** @return Expression to decode a value. Called only when decoders are generated and [[inlineDecode]] is set. */
  def decode(reader: Expr[JsonReader])(using Quotes): Expr[T]

  /** @return String representation of this codec expression */
  def show(using Quotes): String
}

private class BuilderImpl[T](raw: Expr[Any]) extends CodecExpr.Builder[T] { outer =>
  private type EncodeFn = Quotes ?=> (Expr[JsonWriter], Expr[T]) => Expr[Unit]
  private type DecodeFn = Quotes ?=> Expr[JsonReader] => Expr[T]

  private var _encode: Option[EncodeFn] = None
  private var _decode: Option[DecodeFn] = None

  def encode(f: EncodeFn): this.type = {
    _encode = Some(f)
    this
  }

  def decode(f: DecodeFn): this.type = {
    _decode = Some(f)
    this
  }

  def build(): CodecExpr[T] =
    new CodecExpr[T] {
      def raw: Expr[Any] = outer.raw
      def inlineEncode: Boolean = _encode.isDefined
      def inlineDecode: Boolean = _decode.isDefined
      def encode(writer: Expr[JsonWriter], value: Expr[T])(using Quotes): Expr[Unit] =
        _encode
          .getOrElse(throw new IllegalStateException("No inline encoder is defined"))
          .apply(writer, value)

      def decode(reader: Expr[JsonReader])(using Quotes): Expr[T] =
        _decode
          .getOrElse(throw new IllegalStateException("No inline decoder is defined"))
          .apply(reader)

      def show(using Quotes): String = {
        val sb = new mutable.StringBuilder()
        sb ++= "CodecExpr {\n"
        sb ++= "  raw = " ++= outer.raw.show ++= ",\n"
        sb ++= "  inlineEncode = " ++= _encode.isDefined.toString ++= ",\n"
        sb ++= "  inlineDecode = " ++= _decode.isDefined.toString ++= "\n"
        sb ++= "}"
        sb.result()
      }
    }
}
