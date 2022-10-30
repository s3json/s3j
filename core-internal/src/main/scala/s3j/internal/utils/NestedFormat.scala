package s3j.internal.utils

import s3j.format.{JsonDecoder, JsonEncoder}
import s3j.internal.utils.GenerationMode

import scala.quoted.{Expr, Quotes, Type}

object NestedFormat {
  def lookup[T](mode: GenerationMode)(using q: Quotes, t: Type[T]): NestedFormat[T] = {
    import q.reflect.*
    Implicits.search(mode.appliedType(TypeRepr.of[T])) match {
      case s: ImplicitSearchSuccess => new NestedFormat(mode, s.tree.asExpr)
      case f: ImplicitSearchFailure =>
        report.errorAndAbort(s"Failed to find ${mode.toString.toLowerCase} for nested type ${Type.show[T]}: " +
          f.explanation)
    }
  }
}

class NestedFormat[T](mode: GenerationMode, expr: Expr[Any])(using Type[T]) {
  def decoder(using Quotes): Expr[JsonDecoder[T]] = {
    if (!mode.generateDecoders) {
      throw new RuntimeException(s"Mode $mode does not generate decoders")
    }

    expr.asExprOf[JsonDecoder[T]]
  }

  def encoder(using Quotes): Expr[JsonEncoder[T]] = {
    if (!mode.generateEncoders) {
      throw new RuntimeException(s"Mode $mode does not generate encoders")
    }

    expr.asExprOf[JsonEncoder[T]]
  }
}
